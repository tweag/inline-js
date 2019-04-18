import path from "path";
import process from "process";
import { StringDecoder } from "string_decoder";
import url from "url";
import vm from "vm";
import { Worker } from "worker_threads";

import context_global from "./context.mjs";

process.on("uncaughtException", err => {
  process.stderr.write(err.stack);
  throw err;
});

const ctx = vm.createContext(context_global),
  decoder = new StringDecoder("utf8"),
  shared_futex = new Int32Array(new SharedArrayBuffer(4)),
  shared_flag = new Int32Array(new SharedArrayBuffer(4)),
  shared_msg_len = new Int32Array(new SharedArrayBuffer(4)),
  shared_msg_buf = new Uint8Array(new SharedArrayBuffer(1048576)),
  ipc = new Worker(
    path.join(
      path.dirname(url.fileURLToPath(import.meta.url)),
      "transport.mjs"
    ),
    {
      workerData: [
        Number.parseInt(process.argv[process.argv.length - 1]),
        Number.parseInt(process.argv[process.argv.length - 2]),
        shared_futex,
        shared_flag,
        shared_msg_len,
        shared_msg_buf
      ]
    }
  );

function sendMsg(msg_id, ret_tag, is_err, result) {
  ipc.postMessage([
    msg_id,
    ret_tag,
    is_err,
    ret_tag === 1 ? ctx.JSVal.newJSVal(result) : result
  ]);
}

ipc.on("message", async ([msg_id, msg_tag, buf]) => {
  try {
    buf = Buffer.from(buf);
    switch (msg_tag) {
      case 0: {
        const ret_tag = buf.readUInt32LE(0),
          eval_timeout = buf.readUInt32LE(4),
          resolve_timeout = buf.readUInt32LE(8),
          msg_content = decoder.end(buf.slice(12)),
          eval_options = {
            displayErrors: true,
            importModuleDynamically: spec => import(spec)
          };
        if (eval_timeout) eval_options.timeout = eval_timeout;
        const eval_result = vm.runInContext(msg_content, ctx, eval_options),
          raw_promise = Promise.resolve(
            eval_result === undefined ? null : eval_result
          ),
          promise = resolve_timeout
            ? Promise.race([
                raw_promise,
                new Promise((_, reject) =>
                  setTimeout(reject, resolve_timeout, "")
                )
              ])
            : raw_promise,
          promise_result = await promise;
        sendMsg(msg_id, ret_tag, false, promise_result);
        break;
      }
      case 1: {
        sendMsg(msg_id, 1, false, buf);
        break;
      }
      case 2: {
        const import_path = decoder.end(buf),
          import_url = url.pathToFileURL(import_path).href,
          import_result = await import(import_url);
        sendMsg(msg_id, 1, false, import_result);
        break;
      }
      case 3: {
        const hs_func_ref = buf.readUInt32LE(0);
        sendMsg(
          msg_id,
          1,
          false,
          (...args) =>
            new Promise((resolve, reject) => {
              const callback_id = ctx.JSVal.newJSVal([resolve, reject]);
              sendMsg(callback_id, 3, false, [hs_func_ref, args]);
            })
        );
        break;
      }
      case 4: {
        const [resolve, reject] = ctx.JSVal.takeJSVal(msg_id),
          is_err = Boolean(buf.readUInt32LE(0)),
          hs_result = buf.slice(4);
        (is_err ? reject : resolve)(hs_result);
        break;
      }
      case 5: {
        const hs_func_ref = buf.readUInt32LE(0);
        sendMsg(msg_id, 1, false, (...args) => {
          Atomics.store(shared_futex, 0, 0);
          sendMsg(0, 3, false, [hs_func_ref, args]);
          Atomics.wait(shared_futex, 0, 0);
          const is_err = Boolean(Atomics.load(shared_flag)),
            result = Buffer.from(
              shared_msg_buf.buffer,
              0,
              Atomics.load(shared_msg_len, 0)
            );
          if (is_err) throw result;
          else return result;
        });
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
