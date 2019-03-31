import process from "process";
import readline from "readline";
import { Transport } from "./transport.mjs";
import vm from "vm";

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

function sendMsg(msg) {
  const s = JSON.stringify(msg) + "\n";
  process.stdout.write(s, "utf8");
}

process.stdin.setEncoding("utf8");
const rl = readline.createInterface({
  input: process.stdin,
  terminal: false,
  historySize: 0,
  prompt: "",
  crlfDelay: Infinity
});
rl.on("line", async raw_msg => {
  const [
    msg_id,
    msg_tag,
    msg_content,
    eval_timeout,
    resolve_timeout,
    is_async
  ] = JSON.parse(raw_msg);
  try {
    switch (msg_tag) {
      case 0: {
        if (is_async) {
          const promise = vm.runInContext(
            msg_content,
            ctx,
            extendObject({ displayErrors: true }, eval_timeout, {
              timeout: eval_timeout
            })
          );
          sendMsg([
            msg_id,
            0,
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
          ]);
        } else {
          sendMsg([
            msg_id,
            0,
            false,
            noUndefined(
              vm.runInContext(
                msg_content,
                ctx,
                extendObject({ displayErrors: true }, eval_timeout, {
                  timeout: eval_timeout
                })
              )
            )
          ]);
        }
        break;
      }
      default: {
        throw ["parsing SendMsg failed: ", raw_msg];
      }
    }
  } catch (err) {
    sendMsg([msg_id, 0, true, err.stack]);
  }
});

sendMsg([0, 0, false, null]);
