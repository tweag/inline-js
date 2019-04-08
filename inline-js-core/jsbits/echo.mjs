import fs from "fs";
import process from "process";
import { Transport } from "./transport.mjs";

process.on("uncaughtException", err => {
  process.stderr.write(err.stack);
  throw err;
});

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
ipc.on("recv", buf => ipc.send(buf));
