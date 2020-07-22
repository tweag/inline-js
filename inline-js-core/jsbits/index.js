"use strict";

const fs = require("fs").promises;
const path = require("path");
const string_decoder = require("string_decoder");
const util = require("util");
const vm = require("vm");
const worker_threads = require("worker_threads");

class JSValContext {
  constructor() {
    this.jsvalMap = new Map();
    this.jsvalLast = 0n;
    Object.seal(this);
  }

  new(x) {
    const i = ++this.jsvalLast;
    this.jsvalMap.set(i, x);
    return i;
  }

  get(i) {
    if (!this.jsvalMap.has(i)) {
      throw new Error(`jsval.get(${i}): invalid key`);
    }
    return this.jsvalMap.get(i);
  }

  free(i) {
    if (!this.jsvalMap.delete(i)) {
      throw new Error(`jsval.free(${i}): invalid key`);
    }

    if (i === this.jsvalLast) {
      if (this.jsvalMap.size > 0) {
        --this.jsvalLast;
        while (!this.jsvalMap.has(this.jsvalLast)) {
          --this.jsvalLast;
        }
      } else {
        this.jsvalLast = 0n;
      }
    }
  }

  clear() {
    this.jsvalMap.clear();
    this.jsvalLast = 0n;
  }
}

class MainContext {
  constructor() {
    process.on("uncaughtException", (err) => this.onUncaughtException(err));
    const exportSyncArrayBuffer = new SharedArrayBuffer(
      8 +
        Number.parseInt(process.env.INLINE_JS_EXPORT_SYNC_BUFFER_SIZE, 10) *
          0x100000
    );
    this.exportSyncFlags = new Int32Array(exportSyncArrayBuffer, 0, 1);
    this.exportSyncBuffer = Buffer.from(exportSyncArrayBuffer, 4);
    this.worker = new worker_threads.Worker(__filename, {
      stdout: true,
      workerData: exportSyncArrayBuffer,
    });
    this.worker.on("message", (buf_msg) => this.onWorkerMessage(buf_msg));
    this.recvLoop();
    Object.freeze(this);
  }

  recvLoop() {
    let buf = Buffer.allocUnsafe(0);
    process.stdin.on("data", (c) => {
      buf = Buffer.concat([buf, c]);
      while (true) {
        if (buf.length < 8) return;
        const len = Number(buf.readBigUInt64LE(0));
        if (buf.length < 8 + len) return;
        const buf_msg = buf.slice(8, 8 + len);
        buf = buf.slice(8 + len);

        Atomics.wait(this.exportSyncFlags, 0, 2);
        const export_sync_state = Atomics.load(this.exportSyncFlags, 0);

        if (export_sync_state === 1) {
          this.exportSyncBuffer.writeUInt32LE(buf_msg.length, 0);
          buf_msg.copy(this.exportSyncBuffer, 4);
          Atomics.store(this.exportSyncFlags, 0, 2);
          Atomics.notify(this.exportSyncFlags, 0, 1);
          continue;
        }

        if (buf_msg.readUInt8(0) === 4) {
          process.stdin.unref();
        }
        this.worker.postMessage(buf_msg);
      }
    });
  }

  send(buf_msg) {
    return new Promise((resolve, reject) => {
      const buf_send = Buffer.allocUnsafe(8 + buf_msg.length);
      buf_send.writeBigUInt64LE(BigInt(buf_msg.length));
      buf_msg.copy(buf_send, 8);
      process.stdout.write(buf_send, (err) => (err ? reject(err) : resolve()));
    });
  }

  onWorkerMessage(buf_msg) {
    this.send(bufferFromArrayBufferView(buf_msg));
  }

  onUncaughtException(err) {
    const err_str = `${err.stack ? err.stack : err}`;
    const err_buf = Buffer.from(err_str, "utf-8");
    const resp_buf = Buffer.allocUnsafe(9 + err_buf.length);
    resp_buf.writeUInt8(2, 0);
    resp_buf.writeBigUInt64LE(BigInt(err_buf.length), 1);
    err_buf.copy(resp_buf, 9);
    this.send(resp_buf).finally(() => process.exit(1));
  }
}

class WorkerContext {
  constructor() {
    const exportSyncArrayBuffer = worker_threads.workerData;
    this.exportSyncFlags = new Int32Array(exportSyncArrayBuffer, 0, 1);
    this.exportSyncBuffer = Buffer.from(exportSyncArrayBuffer, 4);
    this.decoder = new string_decoder.StringDecoder("utf-8");
    this.jsval = new JSValContext();
    this.hsCtx = new JSValContext();
    (async () => {
      if (process.env.INLINE_JS_NODE_MODULES) {
        await fs.symlink(
          process.env.INLINE_JS_NODE_MODULES,
          path.join(__dirname, "node_modules"),
          "dir"
        );
      }
      worker_threads.parentPort.on("message", (buf_msg) =>
        this.onParentMessage(buf_msg)
      );
    })();
    Object.freeze(this);
  }

  toJS(buf, p) {
    const jsval_tmp = [];
    const expr_segs_len = Number(buf.readBigUInt64LE(p));
    p += 8;
    let expr = "";
    let has_code = false;
    for (let i = 0; i < expr_segs_len; ++i) {
      const expr_seg_type = buf.readUInt8(p);
      p += 1;
      switch (expr_seg_type) {
        case 0: {
          // Code
          const expr_seg_len = Number(buf.readBigUInt64LE(p));
          p += 8;
          expr = `${expr}${this.decoder.end(buf.slice(p, p + expr_seg_len))}`;
          p += expr_seg_len;
          has_code = has_code || Boolean(expr_seg_len);
          break;
        }

        case 1: {
          // BufferLiteral
          const buf_len = Number(buf.readBigUInt64LE(p));
          p += 8;
          const buf_id = jsval_tmp.push(buf.slice(p, p + buf_len)) - 1;
          expr = `${expr}__t${buf_id.toString(36)}`;
          p += buf_len;
          break;
        }

        case 2: {
          // StringLiteral
          const buf_len = Number(buf.readBigUInt64LE(p));
          p += 8;
          const str_id =
            jsval_tmp.push(this.decoder.end(buf.slice(p, p + buf_len))) - 1;
          expr = `${expr}__t${str_id.toString(36)}`;
          p += buf_len;
          break;
        }

        case 3: {
          // JSONLiteral
          const buf_len = Number(buf.readBigUInt64LE(p));
          p += 8;
          const json_id =
            jsval_tmp.push(
              JSON.parse(this.decoder.end(buf.slice(p, p + buf_len)))
            ) - 1;
          expr = `${expr}__t${json_id.toString(36)}`;
          p += buf_len;
          break;
        }

        case 4: {
          // JSValLiteral
          const jsval_id =
            jsval_tmp.push(this.jsval.get(buf.readBigUInt64LE(p))) - 1;
          expr = `${expr}__t${jsval_id.toString(36)}`;
          p += 8;
          break;
        }

        default: {
          throw new Error(`toJS failed: ${buf}`);
        }
      }
    }

    let result;

    if (!has_code && jsval_tmp.length === 1) {
      result = jsval_tmp[0];
    } else {
      let expr_params = "require";
      for (let i = 0; i < jsval_tmp.length; ++i) {
        expr_params = `${expr_params}, __t${i.toString(36)}`;
      }
      expr = `(${expr_params}) => (\n${expr}\n)`;
      result = vm.runInThisContext(expr, {
        lineOffset: -1,
        importModuleDynamically: (spec) => import(spec),
      })(require, ...jsval_tmp);
    }

    return { p: p, result: result };
  }

  fromJS(val, val_type) {
    switch (val_type) {
      case 0: {
        // RawNone
        return Buffer.allocUnsafe(0);
      }

      case 1: {
        // RawBuffer
        return Buffer.isBuffer(val)
          ? val
          : util.types.isArrayBufferView(val)
          ? bufferFromArrayBufferView(val)
          : Buffer.from(val);
      }

      case 2: {
        // RawJSON
        return Buffer.from(JSON.stringify(val), "utf-8");
      }

      case 3: {
        // RawJSVal
        const val_buf = Buffer.allocUnsafe(8);
        val_buf.writeBigUInt64LE(this.jsval.new(val), 0);
        return val_buf;
      }

      default: {
        throw new Error(`fromJS: invalid type ${val_type}`);
      }
    }
  }

  async onParentMessage(buf_msg) {
    const resp_buf = await this.handleParentMessage(buf_msg);
    if (resp_buf) {
      worker_threads.parentPort.postMessage(resp_buf);
    }
  }

  handleParentMessage(buf_msg) {
    buf_msg = bufferFromArrayBufferView(buf_msg);
    let p = 0;
    const msg_tag = buf_msg.readUInt8(p);
    p += 1;
    switch (msg_tag) {
      case 0: {
        // JSEvalRequest
        const req_id = buf_msg.readBigUInt64LE(p);
        p += 8;
        try {
          const r = this.toJS(buf_msg, p);
          p = r.p;

          const on_eval_result = (eval_result) => {
            const return_type = buf_msg.readUInt8(p);
            p += 1;
            const eval_result_buf = this.fromJS(eval_result, return_type);
            const resp_buf = Buffer.allocUnsafe(18 + eval_result_buf.length);
            resp_buf.writeUInt8(0, 0);
            resp_buf.writeBigUInt64LE(req_id, 1);
            resp_buf.writeUInt8(1, 9);
            resp_buf.writeBigUInt64LE(BigInt(eval_result_buf.length), 10);
            eval_result_buf.copy(resp_buf, 18);
            return resp_buf;
          };

          return isPromise(r.result)
            ? r.result
                .then(on_eval_result)
                .catch((err) => this.onEvalError(req_id, err))
            : on_eval_result(r.result);
        } catch (err) {
          // EvalError
          return this.onEvalError(req_id, err);
        }
      }

      case 1: {
        // HSExportRequest
        const is_sync = Boolean(buf_msg.readUInt8(p));
        p += 1;
        const req_id = buf_msg.readBigUInt64LE(p);
        p += 8;
        try {
          const hs_func_id = buf_msg.readBigUInt64LE(p);
          p += 8;
          const hs_func_args_len = Number(buf_msg.readBigUInt64LE(p));
          p += 8;
          const hs_func_args_type = [];
          for (let i = 0; i < hs_func_args_len; ++i) {
            const r = this.toJS(buf_msg, p);
            p = r.p;
            const raw_type = buf_msg.readUInt8(p);
            p += 1;
            hs_func_args_type.push([r.result, raw_type]);
          }

          const js_func = (...js_args) => {
            if (js_args.length !== hs_func_args_len) {
              throw new Error(
                `inline-js export function error: arity mismatch, expected ${hs_func_args_len} arguments, got ${js_args.length}`
              );
            }

            const hs_args_buf = [];
            for (let i = 0; i < hs_func_args_len; ++i) {
              hs_args_buf.push(
                this.fromJS(
                  hs_func_args_type[i][0](js_args[i]),
                  hs_func_args_type[i][1]
                )
              );
            }

            const req_buf = Buffer.allocUnsafe(
              26 +
                hs_func_args_len * 8 +
                hs_args_buf.reduce((acc, hs_arg) => acc + hs_arg.length, 0)
            );

            const hs_eval_req_promise = newPromise();
            const hs_eval_req_id = this.hsCtx.new(hs_eval_req_promise);

            req_buf.writeUInt8(1, 0);
            req_buf.writeUInt8(Number(is_sync), 1);
            req_buf.writeBigUInt64LE(hs_eval_req_id, 2);
            req_buf.writeBigUInt64LE(hs_func_id, 10);
            req_buf.writeBigUInt64LE(BigInt(hs_func_args_len), 18);
            let p = 26;
            for (const hs_arg of hs_args_buf) {
              req_buf.writeBigUInt64LE(BigInt(hs_arg.length), p);
              p += 8;
              hs_arg.copy(req_buf, p);
              p += hs_arg.length;
            }

            if (is_sync) {
              Atomics.store(this.exportSyncFlags, 0, 1);
            }

            worker_threads.parentPort.postMessage(req_buf);

            if (is_sync) {
              Atomics.wait(this.exportSyncFlags, 0, 1);
              const buf_msg_len = this.exportSyncBuffer.readUInt32LE(0);
              const buf_msg = Buffer.from(
                this.exportSyncBuffer.slice(4, 4 + buf_msg_len)
              );
              Atomics.store(this.exportSyncFlags, 0, 0);

              let p = 10;
              const hs_eval_resp_tag = buf_msg.readUInt8(p);
              p += 1;
              switch (hs_eval_resp_tag) {
                case 0: {
                  const err_len = Number(buf_msg.readBigUInt64LE(p));
                  p += 8;
                  err = new Error(
                    this.decoder.end(buf_msg.slice(p, p + err_len))
                  );
                  p += err_len;
                  throw err;
                }

                case 1: {
                  const r = this.toJS(buf_msg, p);
                  p = r.p;
                  return r.result;
                }

                default: {
                  throw new Error(`inline-js invalid message ${buf_msg}`);
                }
              }
            } else {
              return hs_eval_req_promise;
            }
          };

          const js_func_buf = this.fromJS(js_func, 3);
          const resp_buf = Buffer.allocUnsafe(18 + js_func_buf.length);
          resp_buf.writeUInt8(0, 0);
          resp_buf.writeBigUInt64LE(req_id, 1);
          resp_buf.writeUInt8(1, 9);
          resp_buf.writeBigUInt64LE(BigInt(js_func_buf.length), 10);
          js_func_buf.copy(resp_buf, 18);
          return resp_buf;
        } catch (err) {
          // EvalError
          return this.onEvalError(req_id, err);
        }
      }

      case 2: {
        // HSEvalResponse
        const is_sync = Boolean(buf_msg.readUInt8(p));
        p += 1;
        const hs_eval_resp_id = buf_msg.readBigUInt64LE(p);
        p += 8;
        const hs_eval_resp_promise = this.hsCtx.get(hs_eval_resp_id);
        this.hsCtx.free(hs_eval_resp_id);
        const hs_eval_resp_tag = buf_msg.readUInt8(p);
        p += 1;
        switch (hs_eval_resp_tag) {
          case 0: {
            const err_len = Number(buf_msg.readBigUInt64LE(p));
            p += 8;
            err = new Error(this.decoder.end(buf_msg.slice(p, p + err_len)));
            p += err_len;
            hs_eval_resp_promise.reject(err);
            break;
          }

          case 1: {
            const r = this.toJS(buf_msg, p);
            p = r.p;
            hs_eval_resp_promise.resolve(r.result);
            break;
          }

          default: {
            throw new Error(`inline-js invalid message ${buf_msg}`);
          }
        }
        return;
      }

      case 3: {
        // JSValFree
        const jsval_id = buf_msg.readBigUInt64LE(p);
        p += 8;
        this.jsval.free(jsval_id);
        return;
      }

      case 4: {
        // Close
        this.jsval.clear();
        worker_threads.parentPort.unref();
        return;
      }

      default: {
        throw new Error(`inline-js invalid message ${buf_msg}`);
      }
    }
  }

  onEvalError(req_id, err) {
    const err_str = `${err.stack ? err.stack : err}`;
    const err_buf = Buffer.from(err_str, "utf-8");
    const resp_buf = Buffer.allocUnsafe(18 + err_buf.length);
    resp_buf.writeUInt8(0, 0);
    resp_buf.writeBigUInt64LE(req_id, 1);
    resp_buf.writeUInt8(0, 9);
    resp_buf.writeBigUInt64LE(BigInt(err_buf.length), 10);
    err_buf.copy(resp_buf, 18);
    return resp_buf;
  }
}

function newPromise() {
  let promise_resolve, promise_reject;
  const p = new Promise((resolve, reject) => {
    promise_resolve = resolve;
    promise_reject = reject;
  });
  p.resolve = promise_resolve;
  p.reject = promise_reject;
  return p;
}

function isPromise(obj) {
  return Boolean(obj) && typeof obj.then === "function";
}

function bufferFromArrayBufferView(a) {
  return Buffer.from(a.buffer, a.byteOffset, a.byteLength);
}

if (worker_threads.isMainThread) {
  new MainContext();
} else {
  new WorkerContext();
}
