"use strict";

const vm = require("vm");
const app = require("express")();

app.use(require("body-parser").json());

app.post("/eval", (req, res) => {
    try {
        res.json({success: new vm.Script(req.body.code).runInThisContext()});
    } catch (err) {
        res.json({error: err.stack});
    }
});

app.listen(require("minimist")(process.argv.slice(2)).port);
