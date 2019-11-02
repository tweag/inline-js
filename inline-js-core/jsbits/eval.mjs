import module from "module";
import process from "process";
import { StringDecoder } from "string_decoder";
import vm from "vm";
import JSVal from "./jsval.mjs";
import { pipeRead, pipeWrite } from "./pipe.mjs";

function errorStringify(err) {
  return err.stack ? err.stack : `${err}`;
}

process.on("unhandledRejection", err => {
  process.stderr.write(errorStringify(err));
  throw err;
});

const decoder = new StringDecoder("utf8"),
  node_read_fd = Number.parseInt(process.argv[process.argv.length - 1]),
  node_write_fd = Number.parseInt(process.argv[process.argv.length - 2]);

function bufferFromU32(x) {
  const buf = Buffer.allocUnsafe(4);
  buf.writeUInt32LE(x, 0);
  return buf;
}

global.JSVal = JSVal;
global.require = module.createRequire(import.meta.url);

function postHostMessage(msg_id, is_err, result) {
  const result_buf = Buffer.from(result),
    msg_buf = Buffer.allocUnsafe(12 + result_buf.length);
  msg_buf.writeUInt32LE(8 + result_buf.length, 0);
  msg_buf.writeUInt32LE(msg_id, 4);
  msg_buf.writeUInt32LE(Number(is_err), 8);
  result_buf.copy(msg_buf, 12);
  pipeWrite(node_write_fd, msg_buf);
}

async function handleHostMessage(msg_buf) {
  const msg_id = msg_buf.readUInt32LE(0),
    msg_tag = msg_buf.readUInt32LE(4),
    buf = msg_buf.slice(8);
  try {
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
        postHostMessage(
          msg_id,
          false,
          ret_tag === 1
            ? bufferFromU32(JSVal.newJSVal(promise_result))
            : ret_tag === 2
            ? Buffer.allocUnsafe(0)
            : promise_result
        );
        break;
      }
      case 1: {
        postHostMessage(msg_id, false, bufferFromU32(JSVal.newJSVal(buf)));
        break;
      }
      default: {
        throw new Error(`Unsupported tag ${msg_tag}`);
      }
    }
  } catch (err) {
    postHostMessage(msg_id, true, errorStringify(err));
  }
}

const msg_len_buf = Buffer.allocUnsafe(4),
  shutdown_buf = Buffer.from("SHUTDOWN");

async function onHostMessage() {
  await pipeRead(node_read_fd, msg_len_buf, 4);
  const msg_len = msg_len_buf.readUInt32LE(0),
    msg_buf = Buffer.allocUnsafe(msg_len);
  await pipeRead(node_read_fd, msg_buf, msg_len);
  if (!msg_buf.equals(shutdown_buf)) setImmediate(onHostMessage);
  setImmediate(handleHostMessage, msg_buf);
}

onHostMessage();
