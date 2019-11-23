"use strict";

const { TextDecoder } = require("util"),
  vm = require("vm"),
  JSVal = require("./jsval.js"),
  { pipeRead, pipeWrite } = require("./pipe.js");

function errorStringify(err) {
  return err.stack ? err.stack : `${err}`;
}

process.on("unhandledRejection", err => {
  process.stderr.write(errorStringify(err));
  throw err;
});

const decoder = new TextDecoder("utf-8", { fatal: true }),
  node_read_fd = Number.parseInt(process.argv[process.argv.length - 1]),
  node_write_fd = Number.parseInt(process.argv[process.argv.length - 2]);

function bufferFromU32(x) {
  const view = new DataView(new ArrayBuffer(4));
  view.setUint32(0, x, true);
  return view.buffer;
}

global.JSVal = JSVal;
global.require = require;

function postHostMessage(msg_id, is_err, result) {
  const result_buf = Buffer.from(result),
    msg_view = new DataView(new ArrayBuffer(12 + result_buf.length));
  msg_view.setUint32(0, 8 + result_buf.length, true);
  msg_view.setUint32(4, msg_id, true);
  msg_view.setUint32(8, Number(is_err), true);
  result_buf.copy(new Uint8Array(msg_view.buffer), 12);
  pipeWrite(node_write_fd, msg_view);
}

async function handleHostMessage(msg_view) {
  const msg_id = msg_view.getUint32(0, true),
    msg_tag = msg_view.getUint32(4, true),
    buf = msg_view.buffer.slice(8);
  try {
    switch (msg_tag) {
      case 0: {
        const ret_tag = new DataView(buf).getUint32(0, true),
          msg_content = decoder.decode(buf.slice(4)),
          eval_options = {
            displayErrors: true,
            importModuleDynamically: spec => import(spec)
          };
        const eval_code = `(async () => (${msg_content}))()`;
        const promise = vm.runInThisContext(eval_code, eval_options),
          promise_result = await promise;
        postHostMessage(
          msg_id,
          false,
          ret_tag === 1
            ? bufferFromU32(JSVal.newJSVal(promise_result))
            : ret_tag === 2
            ? new ArrayBuffer(0)
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

async function onHostMessage() {
  const msg_len_view = new DataView(new ArrayBuffer(4));
  await pipeRead(node_read_fd, msg_len_view, 4);
  const msg_len = msg_len_view.getUint32(0, true);
  if (msg_len) {
    const msg_view = new DataView(new ArrayBuffer(msg_len));
    await pipeRead(node_read_fd, msg_view, msg_len);
    setImmediate(onHostMessage);
    setImmediate(handleHostMessage, msg_view);
  }
}

onHostMessage();
