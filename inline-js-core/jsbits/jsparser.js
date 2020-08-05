"use strict";

const babelParser = require("@babel/parser");
const babelTraverse = require("@babel/traverse");

function parse(input) {
  let ast, has_await, is_expr, err;

  try {
    ast = babelParser.parseExpression(input, { sourceType: "script" });
    has_await = false;
    is_expr = true;
  } catch (_) {
    try {
      ast = babelParser.parse(input, {
        allowReturnOutsideFunction: true,
        sourceType: "script",
      });
      has_await = false;
      is_expr = false;
    } catch (_) {
      try {
        ast = babelParser.parseExpression(input, {
          allowAwaitOutsideFunction: true,
          sourceType: "script",
        });
        has_await = true;
        is_expr = true;
      } catch (_) {
        try {
          ast = babelParser.parse(input, {
            allowAwaitOutsideFunction: true,
            allowReturnOutsideFunction: true,
            sourceType: "script",
          });
          has_await = true;
          is_expr = false;
        } catch (e) {
          err = e.stack ? e.stack : e;
        }
      }
    }
  }

  if (err) {
    const err_buf = Buffer.from(err, "utf-8");
    const resp_buf = Buffer.allocUnsafe(9 + err_buf.length);
    resp_buf.writeUInt8(0, 0);
    resp_buf.writeBigUInt64LE(BigInt(err_buf.length), 1);
    err_buf.copy(resp_buf, 9);
    return resp_buf;
  }
}

module.exports = parse;
