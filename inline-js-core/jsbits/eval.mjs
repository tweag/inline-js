import process from "process";
import vm from "vm";

import { Transport } from "./transport.mjs";

process.on("uncaughtException", err => {
  process.stderr.write(err.stack);
  throw err;
});

const __jsrefs = [undefined];

global.JSVal = class {
  static newJSVal(v) {
    return __jsrefs.push(v) - 1;
  }
  static deRefJSVal(p) {
    return __jsrefs[p];
  }
};

const ctx = vm.createContext(Object.assign({}, global));

const ipc = new Transport(process.stdin, process.stdout);

function sendMsg(msg_id, is_err, result) {
  if (result === undefined) result = null;
  const result_buf = Buffer.from(JSON.stringify(result)),
    msg_buf = Buffer.allocUnsafe(8 + result_buf.length);
  msg_buf.writeUInt32LE(msg_id, 0);
  msg_buf.writeUInt32LE(Number(is_err), 4);
  result_buf.copy(msg_buf, 8);
  ipc.send(msg_buf);
}

const decoder = new TextDecoder("utf-8", { fatal: true });

ipc.on("recv", async buf => {
  const msg_id = buf.readUInt32LE(0);
  try {
    const msg_tag = buf.readUInt32LE(4);
    switch (msg_tag) {
      case 0: {
        const is_async = Boolean(buf.readUInt32LE(8)),
          eval_timeout = buf.readUInt32LE(12),
          resolve_timeout = buf.readUInt32LE(16),
          msg_content = decoder.decode(buf.slice(20)),
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
          sendMsg(msg_id, false, promise_result);
        } else {
          sendMsg(msg_id, false, eval_result);
        }
        break;
      }
      default: {
        throw new Error(`Unsupported tag ${msg_tag}`);
      }
    }
  } catch (err) {
    sendMsg(msg_id, true, err.toString());
  }
});
