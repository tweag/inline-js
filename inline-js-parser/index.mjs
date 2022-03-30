import * as acorn from "acorn";
import getStdin from "get-stdin";

function tokenize(s) {
  const toks = new Set();
  acorn.parse(s, {
    ecmaVersion: "latest",
    onToken: (tok) => {
      if (tok.type.label === "name" && tok.value.startsWith("$")) {
        toks.add(tok.value.slice(1));
      }
    },
  });
  return toks;
}

const s = await getStdin();

let toks, is_expr;

try {
  toks = tokenize(`async () => (${s})`);
  is_expr = true;
} catch (_) {
  toks = tokenize(`async () => {${s}}`);
  is_expr = false;
}

let o = `${is_expr ? "True" : "False"}\n`;
toks.forEach((tok) => {
  o = `${o}${tok}\n`;
});
process.stdout.write(o);
