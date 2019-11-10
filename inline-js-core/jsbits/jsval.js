"use strict";

class JSValManager {
  constructor() {
    this.map = new Map([[0, null]]);
    this.last = 0;
    Object.seal(this);
  }

  newJSVal(v) {
    if (v === undefined || v === null) return 0;
    const k = (this.last += 2);
    this.map.set(k, v);
    return k;
  }

  deRefJSVal(k) {
    this.checkJSVal(k);
    return this.map.get(k);
  }

  freeJSVal(k) {
    this.checkJSVal(k);
    this.map.delete(k);
  }

  takeJSVal(k) {
    this.checkJSVal(k);
    const v = this.map.get(k);
    this.map.delete(k);
    return v;
  }

  checkJSVal(k) {
    if (typeof k !== "number")
      throw new Error(`Invalid JSVal type ${typeof k}`);
    if (!this.map.has(k)) throw new Error(`Invalid JSVal 0x${k.toString(16)}`);
  }
}

module.exports = new JSValManager();
