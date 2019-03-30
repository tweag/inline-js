import assert from "assert";
import { EventEmitter } from "events";

export class Transport extends EventEmitter {
  constructor(i, o) {
    super();
    this.i = i;
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
          assert(this.iMsgLen > 0);
          this.iRest = this.iRest.slice(4);
        }
        if (this.iRest.length < this.iMsgLen) break;
        this.emit("recv", this.iRest.slice(0, this.iMsgLen));
        this.iRest = this.iRest.slice(this.iMsgLen);
        this.iMsgLen = 0;
      }
    });
    this.i.on("error", err => this.emit("error", err));
    this.o.on("error", err => this.emit("error", err));
  }

  send(buf) {
    return new Promise((resolve, reject) => {
      const nbuf = Buffer.allocUnsafe(buf.length + 4);
      nbuf.writeUInt32LE(buf.length);
      buf.copy(nbuf, 4);
      this.o.write(nbuf, err => {
        if (typeof err === "undefined") {
          this.emit("send", buf);
          resolve();
        } else reject(err);
      });
    });
  }
}
