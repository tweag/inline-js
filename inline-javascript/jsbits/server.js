"use strict";

const sandbox = {};
Object.assign(sandbox, global);
sandbox.require = require;

const vm = require("vm");
const context = vm.createContext(sandbox);

const express = require("express");
const app = express();
app.use(express.json({strict: false}));

app.post("/eval", (req, res) => {
    try {
        Promise.resolve(vm.runInContext(req.body, context, {displayErrors: true})).then(val => res.json(val === undefined ? null : val), err => res.status(500).json(err));
    } catch (err) {
        res.status(500).json(err.stack);
    }
});

const server = app.listen(0, "localhost", () => console.log(server.address().port));
