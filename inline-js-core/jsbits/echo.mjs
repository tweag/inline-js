import process from "process";
import { Transport } from "./transport.mjs";

process.on("uncaughtException", err => {
  process.stderr.write(err.stack);
  throw err;
});

const ipc = new Transport(process.stdin, process.stdout);
ipc.on("recv", buf => ipc.send(buf));
