import fs from "fs";
import process from "process";
import vm from "vm";

import { Transport } from "./transport.mjs";

process.on("uncaughtException", err => {
  process.stderr.write(err.stack);
  throw err;
});

const __jsrefs = [null];

global.JSVal = class {
  static newJSVal(v) {
    return v === undefined || v === null ? 0 : __jsrefs.push(v) - 1;
  }
  static deRefJSVal(p) {
    return __jsrefs[p];
  }
};

const ctx = vm.createContext(global);

const ipc = new Transport(
  fs.createReadStream(null, {
    encoding: null,
    fd: Number.parseInt(process.argv[process.argv.length - 1]),
    autoClose: false
  }),
  fs.createWriteStream(null, {
    encoding: null,
    fd: Number.parseInt(process.argv[process.argv.length - 2]),
    autoClose: false
  })
);

function sendMsg(msg_id, ret_tag, is_err, result) {
  try {
    switch (ret_tag) {
      case 0: {
        const result_buf = Buffer.from(result),
          msg_buf = Buffer.allocUnsafe(8 + result_buf.length);
        msg_buf.writeUInt32LE(msg_id, 0);
        msg_buf.writeUInt32LE(Number(is_err), 4);
        result_buf.copy(msg_buf, 8);
        ipc.send(msg_buf);
        break;
      }
      case 1: {
        const msg_buf = Buffer.allocUnsafe(12);
        msg_buf.writeUInt32LE(msg_id, 0);
        msg_buf.writeUInt32LE(0, 4);
        msg_buf.writeUInt32LE(JSVal.newJSVal(result), 8);
        ipc.send(msg_buf);
        break;
      }
      default: {
        throw new Error(`Unsupported ret_tag ${ret_tag}`);
      }
    }
  } catch (err) {
    sendMsg(msg_id, 0, true, err.toString());
  }
}

const decoder = new TextDecoder("utf-8", { fatal: true });

ipc.on("recv", async buf => {
  const msg_id = buf.readUInt32LE(0);
  try {
    const msg_tag = buf.readUInt32LE(4);
    switch (msg_tag) {
      case 0: {
        const ret_tag = buf.readUInt32LE(8),
          is_async = Boolean(buf.readUInt32LE(12)),
          eval_timeout = buf.readUInt32LE(16),
          resolve_timeout = buf.readUInt32LE(20),
          msg_content = decoder.decode(buf.slice(24)),
          eval_options = {
            displayErrors: true,
            importModuleDynamically: spec => import(spec)
          };
        if (eval_timeout) eval_options.timeout = eval_timeout;
        const eval_result = vm.runInContext(msg_content, ctx, eval_options);
        if (is_async) {
          const promise = resolve_timeout
              ? Promise.race([
                  eval_result,
                  new Promise((_, reject) =>
                    setTimeout(reject, resolve_timeout, "")
                  )
                ])
              : eval_result,
            promise_result = await promise;
          sendMsg(msg_id, ret_tag, false, promise_result);
        } else {
          sendMsg(msg_id, ret_tag, false, eval_result);
        }
        break;
      }
      case 1: {
        sendMsg(msg_id, 1, false, buf.slice(8));
        break;
      }
      default: {
        throw new Error(`Unsupported tag ${msg_tag}`);
      }
    }
  } catch (err) {
    sendMsg(msg_id, 0, true, err.toString());
  }
});
