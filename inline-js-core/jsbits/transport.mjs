import fs from "fs";
import { parentPort, workerData } from "worker_threads";

const shared_futex = workerData[2],
  shared_flag = workerData[3],
  shared_msg_len = workerData[4],
  shared_msg_buf = workerData[5];

function bufferFromU32(x) {
  const buf = Buffer.allocUnsafe(4);
  buf.writeUInt32LE(x, 0);
  return buf;
}

class Transport {
  constructor(i, o) {
    this.i = fs.createReadStream(null, {
      encoding: null,
      fd: i,
      autoClose: false
    });
    this.o = o;
    this.iMsgLen = 0;
    this.iRest = Buffer.allocUnsafe(0);
    Object.seal(this);
    this.i.on("data", buf => {
      this.iRest = Buffer.concat([this.iRest, buf]);
      while (true) {
        if (!this.iMsgLen) {
          if (this.iRest.length < 4) break;
          this.iMsgLen = this.iRest.readUInt32LE(0);
          this.iRest = this.iRest.slice(4);
        }
        if (this.iRest.length < this.iMsgLen) break;
        const msg_buf = this.iRest.slice(0, this.iMsgLen),
          msg_id = msg_buf.readUInt32LE(0),
          msg_tag = msg_buf.readUInt32LE(4);
        if (msg_id) {
          parentPort.postMessage([msg_id, msg_tag, msg_buf.slice(8)]);
        } else {
          const is_err = msg_buf.readUInt32LE(8),
            hs_result = msg_buf.slice(12);
          hs_result.copy(shared_msg_buf);
          Atomics.store(shared_msg_len, 0, hs_result.length);
          Atomics.store(shared_flag, 0, is_err);
          Atomics.store(shared_futex, 0, 1);
          Atomics.notify(shared_futex, 0, 1);
        }
        this.iRest = this.iRest.slice(this.iMsgLen);
        this.iMsgLen = 0;
      }
    });
  }
}

const ipc = new Transport(workerData[0], workerData[1]);

parentPort.on("message", ([msg_id, ret_tag, is_err, result]) => {
  let buf;
  switch (ret_tag) {
    case 0: {
      const result_buf = Buffer.from(result),
        msg_buf = Buffer.allocUnsafe(8 + result_buf.length);
      msg_buf.writeUInt32LE(msg_id, 0);
      msg_buf.writeUInt32LE(Number(is_err), 4);
      result_buf.copy(msg_buf, 8);
      buf = msg_buf;
      break;
    }
    case 1: {
      const msg_buf = Buffer.allocUnsafe(12);
      msg_buf.writeUInt32LE(msg_id, 0);
      msg_buf.writeUInt32LE(0, 4);
      msg_buf.writeUInt32LE(result, 8);
      buf = msg_buf;
      break;
    }
    case 2: {
      const msg_buf = Buffer.allocUnsafe(8);
      msg_buf.writeUInt32LE(msg_id, 0);
      msg_buf.writeUInt32LE(0, 4);
      buf = msg_buf;
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
      buf = Buffer.concat(buf_args);
      break;
    }
    default: {
      throw new Error(`Unsupported ret_tag ${ret_tag}`);
    }
  }
  fs.writeSync(ipc.o, bufferFromU32(buf.length));
  fs.writeSync(ipc.o, buf);
});
