import module from "module";
import path from "path";
import process from "process";
import { StringDecoder } from "string_decoder";
import url from "url";
import vm from "vm";
import { Worker } from "worker_threads";
import JSVal from "./jsval.mjs";

function errorStringify(err) {
  return err.stack ? err.stack : `${err}`;
}

process.on("uncaughtException", err => {
  process.stderr.write(errorStringify(err));
  throw err;
});

process.on("unhandledRejection", err => {
  process.stderr.write(errorStringify(err));
  throw err;
});

const decoder = new StringDecoder("utf8"),
  ipc = new Worker(
    path.join(
      path.dirname(url.fileURLToPath(import.meta.url)),
      "transport.mjs"
    ),
    {
      workerData: [
        Number.parseInt(process.argv[process.argv.length - 1]),
        Number.parseInt(process.argv[process.argv.length - 2])
      ]
    }
  );

function bufferFromU32(x) {
  const buf = Buffer.allocUnsafe(4);
  buf.writeUInt32LE(x, 0);
  return buf;
}

function callHSFuncRequestBody(hs_func_ref, args) {
  const buf_args = args.flatMap(arg => {
    const raw_buf = Buffer.from(arg);
    return [bufferFromU32(raw_buf.length), raw_buf];
  });
  buf_args.unshift(bufferFromU32(hs_func_ref), bufferFromU32(args.length));
  return Buffer.concat(buf_args);
}

global.JSVal = JSVal;
global.require = module.createRequire(import.meta.url);

ipc.on("message", async ([msg_id, msg_tag, buf]) => {
  try {
    buf = Buffer.from(buf);
    switch (msg_tag) {
      case 0: {
        const ret_tag = buf.readUInt32LE(0),
          msg_content = decoder.end(buf.slice(4)),
          eval_options = {
            displayErrors: true,
            importModuleDynamically: spec => import(spec)
          };
        const eval_result = vm.runInThisContext(msg_content, eval_options),
          promise = Promise.resolve(
            eval_result === undefined ? null : eval_result
          ),
          promise_result = await promise;
        ipc.postMessage([
          msg_id,
          false,
          ret_tag === 1
            ? bufferFromU32(JSVal.newJSVal(promise_result))
            : ret_tag === 2
            ? Buffer.allocUnsafe(0)
            : promise_result
        ]);
        break;
      }
      case 1: {
        ipc.postMessage([msg_id, false, bufferFromU32(JSVal.newJSVal(buf))]);
        break;
      }
      default: {
        throw new Error(`Unsupported tag ${msg_tag}`);
      }
    }
  } catch (err) {
    ipc.postMessage([msg_id, true, errorStringify(err)]);
  }
});
