"use strict";

(() => {
  function noUndefined(x) {
    return x === undefined ? null : x;
  }

  function extendObject(obj, cond, ext) {
    return cond !== false ? Object.assign(obj, ext) : obj;
  }

  function sendMsg(msg, is_error = false, error_prefix = "") {
    const s = JSON.stringify(msg) + "\n";
    process.stdout.write(s, "utf8");
    if (is_error) {
      process.stderr.write(error_prefix + s, "utf8");
    }
  }

  process.on("uncaughtException", err => {
    sendMsg([0, 0, err], true, "server.js: uncaughtException: ");
  });

  process.stdin.setEncoding("utf8");

  const readline = require("readline");

  const vm = require("vm");

  const rl = readline.createInterface({
    input: process.stdin,
    terminal: false,
    historySize: 0,
    prompt: "",
    crlfDelay: Infinity
  });

  rl.on("line", async raw_msg => {
    try {
      const [msg_id, msg_tag, msg_content] = JSON.parse(raw_msg);
      try {
        switch (msg_tag) {
          case 0: {
            sendMsg([msg_id, 2, msg_content]);
            break;
          }
          case 1: {
            const [eval_code, eval_timeout] = msg_content;
            sendMsg([
              msg_id,
              3,
              noUndefined(
                vm.runInThisContext(
                  eval_code,
                  extendObject({ displayErrors: true }, eval_timeout, {
                    timeout: eval_timeout
                  })
                )
              )
            ]);
            break;
          }
          case 2: {
            const [eval_code, eval_timeout, resolve_timeout] = msg_content;
            const promise = vm.runInThisContext(
              eval_code,
              extendObject({ displayErrors: true }, eval_timeout, {
                timeout: eval_timeout
              })
            );
            sendMsg([
              msg_id,
              3,
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
            break;
          }
          default: {
            throw ["parsing SendMsg failed: ", raw_msg];
          }
        }
      } catch (err) {
        sendMsg([msg_id, 0, err.stack], true, "failed to process SendMsg: ");
      }
    } catch (err) {
      sendMsg([0, 0, err.stack], true, "failed to parse SendMsg: ");
    }
  });

  sendMsg([0, 1, null]);
})();