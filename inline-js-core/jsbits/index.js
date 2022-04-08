"use strict";

const fs = require("fs");
const path = require("path");
const string_decoder = require("string_decoder");
const util = require("util");
const vm = require("vm");

class JSValContext {
  constructor() {
    this.jsvalMap = new Map();
    this.jsvalLast = 0n;
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
    this.worker = new WorkerContext(this);
    this.recvLoop();
  }

  async recvLoop() {
    this.outbound = await fs.promises.open(
      process.env.INLINE_JS_PIPE_OUTBOUND,
      "w"
    );

    await this.worker.ready;

    this.inbound = fs.createReadStream(process.env.INLINE_JS_PIPE_INBOUND);
    let buf = Buffer.allocUnsafe(0);
    inbound.on("data", (c) => {
      buf = Buffer.concat([buf, c]);
      while (true) {
        if (buf.length < 8) return;
        const len = Number(buf.readBigUInt64LE(0));
        if (buf.length < 8 + len) return;
        const buf_msg = buf.slice(8, 8 + len);
        buf = buf.slice(8 + len);

        if (buf_msg.readUInt8(0) === 4) {
          this.inbound.close();
        }

        this.worker.onParentMessage(buf_msg);
      }
    });
  }

  send(buf_msg) {
    return new Promise((resolve, reject) => {
      const buf_send = Buffer.allocUnsafe(8 + buf_msg.length);
      buf_send.writeBigUInt64LE(BigInt(buf_msg.length));
      buf_msg.copy(buf_send, 8);
      this.outbound.write(buf_send, (err) => (err ? reject(err) : resolve()));
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
  constructor(parent) {
    this.parent = parent;
    this.decoder = new string_decoder.StringDecoder("utf-8");
    this.jsval = new JSValContext();
    this.hsCtx = new JSValContext();
    this.ready = (async () => {
      if (process.env.INLINE_JS_NODE_MODULES) {
        await fs.promises.symlink(
          process.env.INLINE_JS_NODE_MODULES,
          path.join(__dirname, "node_modules"),
          "dir"
        );
      }
    })();
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
          expr = `${expr}__${buf_id}`;
          p += buf_len;
          break;
        }

        case 2: {
          // StringLiteral
          const buf_len = Number(buf.readBigUInt64LE(p));
          p += 8;
          const str_id =
            jsval_tmp.push(this.decoder.end(buf.slice(p, p + buf_len))) - 1;
          expr = `${expr}__${str_id}`;
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
          expr = `${expr}__${json_id}`;
          p += buf_len;
          break;
        }

        case 4: {
          // JSValLiteral
          const jsval_id =
            jsval_tmp.push(this.jsval.get(buf.readBigUInt64LE(p))) - 1;
          expr = `${expr}__${jsval_id}`;
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
        expr_params = `${expr_params}, __${i}`;
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

  onParentMessage(buf_msg) {
    const r = this.handleParentMessage(buf_msg);
    if (isPromise(r)) {
      r.then((resp_buf) => {
        if (resp_buf) {
          this.parent.onWorkerMessage(resp_buf);
        }
      });
    } else {
      if (r) {
        this.parent.onWorkerMessage(r);
      }
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
              25 +
                hs_func_args_len * 8 +
                hs_args_buf.reduce((acc, hs_arg) => acc + hs_arg.length, 0)
            );

            const hs_eval_req_promise = newPromise();
            const hs_eval_req_id = this.hsCtx.new(hs_eval_req_promise);

            req_buf.writeUInt8(1, 0);
            req_buf.writeBigUInt64LE(hs_eval_req_id, 1);
            req_buf.writeBigUInt64LE(hs_func_id, 9);
            req_buf.writeBigUInt64LE(BigInt(hs_func_args_len), 17);
            let p = 25;
            for (const hs_arg of hs_args_buf) {
              req_buf.writeBigUInt64LE(BigInt(hs_arg.length), p);
              p += 8;
              hs_arg.copy(req_buf, p);
              p += hs_arg.length;
            }

            this.parent.onWorkerMessage(req_buf);

            return hs_eval_req_promise;
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
  p.resolve = (v) => {
    p.fulfilled = true;
    p.value = v;
    promise_resolve(v);
  };
  p.reject = (err) => {
    p.rejected = true;
    p.reason = err;
    promise_reject(err);
  };
  return p;
}

function isPromise(obj) {
  return Boolean(obj) && typeof obj.then === "function";
}

function bufferFromArrayBufferView(a) {
  return Buffer.from(a.buffer, a.byteOffset, a.byteLength);
}

new MainContext();
