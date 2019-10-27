import { parentPort, workerData } from "worker_threads";
import { pipeRead, pipeWrite } from "./pipe.mjs";

const node_read_fd = workerData[0],
  node_write_fd = workerData[1];

async function onHostMessage() {
  const msg_len_buf = Buffer.allocUnsafe(4);
  await pipeRead(node_read_fd, msg_len_buf, 0, 4);
  const msg_len = msg_len_buf.readUInt32LE(0),
    msg_buf = Buffer.allocUnsafe(msg_len);
  await pipeRead(node_read_fd, msg_buf, 0, msg_len);
  const msg_id = msg_buf.readUInt32LE(0),
    msg_tag = msg_buf.readUInt32LE(4);
  parentPort.postMessage([msg_id, msg_tag, msg_buf.slice(8)]);
  setImmediate(onHostMessage);
}

onHostMessage();

parentPort.on("message", ([msg_id, is_err, result]) => {
  const result_buf = Buffer.from(result),
    msg_buf = Buffer.allocUnsafe(12 + result_buf.length);
  msg_buf.writeUInt32LE(8 + result_buf.length, 0);
  msg_buf.writeUInt32LE(msg_id, 4);
  msg_buf.writeUInt32LE(Number(is_err), 8);
  result_buf.copy(msg_buf, 12);
  pipeWrite(node_write_fd, msg_buf);
});
