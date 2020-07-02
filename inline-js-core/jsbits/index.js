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
    const i = this.jsvalLast++;
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
  }
  clear() {
    this.jsvalMap.clear();
    this.jsvalLast = 0n;
  }
}

class MainContext {
  constructor() {
    process.on("uncaughtException", (err) => this.onUncaughtException(err));
    this.worker = new worker_threads.Worker(__filename, { stdout: true });
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
        if (msgIsClose(buf_msg)) {
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
  async onUncaughtException(err) {
    const err_str = `${err.stack ? err.stack : err}`;
    const err_buf = Buffer.from(err_str, "utf-8");
    const resp_buf = Buffer.allocUnsafe(9 + err_buf.length);
    resp_buf.writeUInt8(1, 0);
    resp_buf.writeBigUInt64LE(BigInt(err_buf.length), 1);
    err_buf.copy(resp_buf, 9);
    await this.send(resp_buf);
    process.exit(1);
  }
}

class WorkerContext {
  constructor() {
    this.jsval = new JSValContext();
    (async () => {
      if (process.env.INLINE_JS_NODE_MODULES) {
        await fs.symlink(
          process.env.INLINE_JS_NODE_MODULES,
          path.join(process.env.INLINE_JS_ROOT, "node_modules"),
          "dir"
        );
      }
      worker_threads.parentPort.on("message", (buf_msg) =>
        this.onParentMessage(buf_msg)
      );
    })();
    Object.seal(this);
  }
  async onParentMessage(buf_msg) {
    buf_msg = bufferFromArrayBufferView(buf_msg);
    let p = 0;
    const msg_tag = buf_msg.readUInt8(p);
    p += 1;
    switch (msg_tag) {
      case 0: {
        // JSEvalRequest
        const jsval_tmp = [];
        let resp_buf;
        const req_id = buf_msg.readBigUInt64LE(p);
        p += 8;
        try {
          const code_segs_len = Number(buf_msg.readBigUInt64LE(p));
          p += 8;
          let code = "";
          for (let i = 0; i < code_segs_len; ++i) {
            const code_seg_type = buf_msg.readUInt8(p);
            p += 1;
            switch (code_seg_type) {
              case 0: {
                // Code
                const code_len = Number(buf_msg.readBigUInt64LE(p));
                p += 8;
                code = `${code}${new string_decoder.StringDecoder("utf-8").end(
                  buf_msg.slice(p, p + code_len)
                )}`;
                p += code_len;
                break;
              }
              case 1: {
                // BufferLiteral
                const buf_len = Number(buf_msg.readBigUInt64LE(p));
                p += 8;
                const buf_id =
                  jsval_tmp.push(buf_msg.slice(p, p + buf_len)) - 1;
                code = `${code}__t${buf_id.toString(36)}`;
                p += buf_len;
                break;
              }
              case 2: {
                // StringLiteral
                const buf_len = Number(buf_msg.readBigUInt64LE(p));
                p += 8;
                const str_id =
                  jsval_tmp.push(
                    new string_decoder.StringDecoder("utf-8").end(
                      buf_msg.slice(p, p + buf_len)
                    )
                  ) - 1;
                code = `${code}__t${str_id.toString(36)}`;
                p += buf_len;
                break;
              }
              case 3: {
                // JSONLiteral
                const buf_len = Number(buf_msg.readBigUInt64LE(p));
                p += 8;
                const json_id =
                  jsval_tmp.push(
                    JSON.parse(
                      new string_decoder.StringDecoder("utf-8").end(
                        buf_msg.slice(p, p + buf_len)
                      )
                    )
                  ) - 1;
                code = `${code}__t${json_id.toString(36)}`;
                p += buf_len;
                break;
              }
              case 4: {
                // JSValLiteral
                const jsval_id =
                  jsval_tmp.push(this.jsval.get(buf_msg.readBigUInt64LE(p))) -
                  1;
                code = `${code}__t${jsval_id.toString(36)}`;
                p += 8;
                break;
              }
              default: {
                throw new Error(`recv: invalid message ${buf_msg}`);
              }
            }
          }

          let code_params = "require";
          for (let i = 0; i < jsval_tmp.length; ++i) {
            code_params = `${code_params}, __t${i.toString(36)}`;
          }
          code = `async (${code_params}) => (\n${code}\n)`;

          const eval_result = await vm.runInThisContext(code, {
            lineOffset: -1,
            importModuleDynamically: (spec) => import(spec),
          })(require, ...jsval_tmp);

          const return_type = buf_msg.readUInt8(p);
          p += 1;
          switch (return_type) {
            case 0: {
              // ReturnNone
              resp_buf = Buffer.allocUnsafe(18);
              resp_buf.writeUInt8(0, 0);
              resp_buf.writeBigUInt64LE(req_id, 1);
              resp_buf.writeUInt8(1, 9);
              resp_buf.writeBigUInt64LE(0n, 10);
              break;
            }
            case 1: {
              // ReturnBuffer
              const eval_result_buf = Buffer.isBuffer(eval_result)
                ? eval_result
                : util.types.isArrayBufferView(eval_result)
                ? bufferFromArrayBufferView(eval_result)
                : Buffer.from(eval_result);
              resp_buf = Buffer.allocUnsafe(18 + eval_result_buf.length);
              resp_buf.writeUInt8(0, 0);
              resp_buf.writeBigUInt64LE(req_id, 1);
              resp_buf.writeUInt8(1, 9);
              resp_buf.writeBigUInt64LE(BigInt(eval_result_buf.length), 10);
              eval_result_buf.copy(resp_buf, 18);
              break;
            }
            case 2: {
              // ReturnJSON
              const eval_result_buf = Buffer.from(
                JSON.stringify(eval_result),
                "utf-8"
              );
              resp_buf = Buffer.allocUnsafe(18 + eval_result_buf.length);
              resp_buf.writeUInt8(0, 0);
              resp_buf.writeBigUInt64LE(req_id, 1);
              resp_buf.writeUInt8(1, 9);
              resp_buf.writeBigUInt64LE(BigInt(eval_result_buf.length), 10);
              eval_result_buf.copy(resp_buf, 18);
              break;
            }
            case 3: {
              // ReturnJSVal
              resp_buf = Buffer.allocUnsafe(26);
              resp_buf.writeUInt8(0, 0);
              resp_buf.writeBigUInt64LE(req_id, 1);
              resp_buf.writeUInt8(1, 9);
              resp_buf.writeBigUInt64LE(8n, 10);
              resp_buf.writeBigUInt64LE(this.jsval.new(eval_result), 18);
              break;
            }
            default: {
              throw new Error(`recv: invalid message ${buf_msg}`);
            }
          }
        } catch (err) {
          // EvalError
          const err_str = `${err.stack ? err.stack : err}`;
          if (process.env.INLINE_JS_EXIT_ON_EVAL_ERROR) {
            process.stderr.write(
              `inline-js eval error, exiting node: ${err_str}\n`,
              () => {
                process.kill(process.pid, "SIGTERM");
              }
            );
          }
          const err_buf = Buffer.from(err_str, "utf-8");
          resp_buf = Buffer.allocUnsafe(18 + err_buf.length);
          resp_buf.writeUInt8(0, 0);
          resp_buf.writeBigUInt64LE(req_id, 1);
          resp_buf.writeUInt8(0, 9);
          resp_buf.writeBigUInt64LE(BigInt(err_buf.length), 10);
          err_buf.copy(resp_buf, 18);
        }
        worker_threads.parentPort.postMessage(resp_buf, [resp_buf.buffer]);
        break;
      }
      case 1: {
        // JSValFree
        const jsval_id = buf_msg.readBigUInt64LE(p);
        p += 8;
        this.jsval.free(jsval_id);
        break;
      }
      case 2: {
        // Close
        worker_threads.parentPort.unref();
        break;
      }
      default: {
        throw new Error(`recv: invalid message ${buf_msg}`);
      }
    }
  }
}

function msgIsClose(buf_msg) {
  return buf_msg.readUInt8(0) === 2;
}

function bufferFromArrayBufferView(a) {
  return Buffer.from(a.buffer, a.byteOffset, a.byteLength);
}

if (worker_threads.isMainThread) {
  new MainContext();
} else {
  new WorkerContext();
}
