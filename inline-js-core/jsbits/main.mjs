import process from "process";
import { IPC } from "./ipc.mjs";

process.on("uncaughtException", err => {
  process.stderr.write(err.stack);
  throw err;
});

const ipc = new IPC(process.stdin, process.stdout);
ipc.on("recv", buf => ipc.send(buf));
