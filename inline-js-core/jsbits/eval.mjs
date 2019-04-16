import fs from "fs";
import process from "process";
import { StringDecoder } from "string_decoder";
import url from "url";
import vm from "vm";

import context_global from "./context.mjs";
import { Transport } from "./transport.mjs";

process.on("uncaughtException", err => {
  process.stderr.write(err.stack);
  throw err;
});

const ctx = vm.createContext(context_global);

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

function bufferFromU32(x) {
  const buf = Buffer.allocUnsafe(4);
  buf.writeUInt32LE(x, 0);
  return buf;
}

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
        msg_buf.writeUInt32LE(ctx.JSVal.newJSVal(result), 8);
        ipc.send(msg_buf);
        break;
      }
      case 2: {
        const msg_buf = Buffer.allocUnsafe(8);
        msg_buf.writeUInt32LE(msg_id, 0);
        msg_buf.writeUInt32LE(0, 4);
        ipc.send(msg_buf);
        break;
      }
      case 3: {
        const [hs_func_ref, args] = result,
          buf_args = args.flatMap(arg => {
            const raw_buf = Buffer.from(arg);
            return [bufferFromU32(raw_buf.length), raw_buf];
          });
        buf_args.unshift(
          bufferFromU32(msg_id),
          bufferFromU32(hs_func_ref),
          bufferFromU32(args.length)
        );
        ipc.send(Buffer.concat(buf_args));
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

const decoder = new StringDecoder("utf8");

ipc.on("recv", async buf => {
  const msg_id = buf.readUInt32LE(0);
  try {
    const msg_tag = buf.readUInt32LE(4);
    switch (msg_tag) {
      case 0: {
        const ret_tag = buf.readUInt32LE(8),
          eval_timeout = buf.readUInt32LE(12),
          resolve_timeout = buf.readUInt32LE(16),
          msg_content = decoder.end(buf.slice(20)),
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
        sendMsg(msg_id, 1, false, buf.slice(8));
        break;
      }
      case 2: {
        const import_path = decoder.end(buf.slice(8)),
          import_url = url.pathToFileURL(import_path).href,
          import_result = await import(import_url);
        sendMsg(msg_id, 1, false, import_result);
        break;
      }
      case 3: {
        const hs_func_ref = buf.readUInt32LE(8);
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
          is_err = Boolean(buf.readUInt32LE(8)),
          hs_result = buf.slice(12);
        (is_err ? reject : resolve)(hs_result);
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
