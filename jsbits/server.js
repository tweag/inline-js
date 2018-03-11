"use strict";

const sandbox = {};
Object.assign(sandbox, global);
sandbox.require = require;

const vm = require("vm");
const context = vm.createContext(sandbox);

const WebSocket = require('ws');
const wss = new WebSocket.Server({host: "127.0.0.1", port: 0, perMessageDeflate: true});

wss.on("listening", () => {
    console.log(wss.address().port);
});

wss.on("connection", ws => {
    const sendjson = obj => ws.send(JSON.stringify(obj), {binary: true});
    ws.on("message", msg_buf => {
        const msg = JSON.parse(msg_buf);
        const reply = obj => sendjson(Object.assign({id: msg.id}, obj));
        try {
            Promise.resolve(vm.runInContext(msg.code, context, {displayErrors: true})).then(val => reply({result: val === undefined ? null : val}, err => reply({error: err})));
        }
        catch (err) {
            reply({error: err.stack});
        }
    });
});
