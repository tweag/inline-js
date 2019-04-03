import process from "process";
import { StringDecoder } from "string_decoder";
import vm from "vm";

import { Transport } from "./transport.mjs";

process.on("uncaughtException", err => {
  process.stderr.write(err.stack);
  throw err;
});

const __jsrefs = [];

global.JSRef = class {
  static newJSRef(v) {
    return __jsrefs.push(v) - 1;
  }
  static deRefJSRef(p) {
    return __jsrefs[p];
  }
};

const ctx = vm.createContext(Object.assign({}, global));

function noUndefined(x) {
  return x === undefined ? null : x;
}

function extendObject(obj, cond, ext) {
  return cond !== false ? Object.assign(obj, ext) : obj;
}

const ipc = new Transport(process.stdin, process.stdout);

function sendMsg(msg_id, is_err, result) {
  const result_buf = Buffer.from(JSON.stringify(result)),
    msg_buf = Buffer.allocUnsafe(8 + result_buf.length);
  msg_buf.writeUInt32LE(msg_id, 0);
  msg_buf.writeUInt32LE(Number(is_err), 4);
  result_buf.copy(msg_buf, 8);
  ipc.send(msg_buf);
}

const decoder = new StringDecoder();

ipc.on("recv", async buf => {
  const raw_msg = decoder.write(buf);
  const [
    msg_id,
    is_async,
    eval_timeout,
    resolve_timeout,
    msg_content
  ] = JSON.parse(raw_msg);
  try {
    if (is_async) {
      const promise = vm.runInContext(
        msg_content,
        ctx,
        extendObject(
          {
            displayErrors: true,
            importModuleDynamically: spec => import(spec)
          },
          eval_timeout,
          { timeout: eval_timeout }
        )
      );
      sendMsg(
        msg_id,
        false,
        noUndefined(
          await (resolve_timeout !== false
            ? Promise.race([
                promise,
                new Promise((_, reject) =>
                  setTimeout(reject, resolve_timeout, "")
                )
              ])
            : promise)
        )
      );
    } else {
      sendMsg(
        msg_id,
        false,
        noUndefined(
          vm.runInContext(
            msg_content,
            ctx,
            extendObject(
              {
                displayErrors: true,
                importModuleDynamically: spec => import(spec)
              },
              eval_timeout,
              { timeout: eval_timeout }
            )
          )
        )
      );
    }
  } catch (err) {
    sendMsg(msg_id, true, err.toString());
  }
});

sendMsg(0, false, null);
