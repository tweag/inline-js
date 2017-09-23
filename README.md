# nodejs-eval

[![Build Status](https://travis-ci.org/TerrorJack/nodejs-eval.svg)](https://travis-ci.org/TerrorJack/nodejs-eval)

Execute Node.js scripts in Haskell.

## Usage

First, add `nodejs-eval` to the [`custom-setup`](https://cabal.readthedocs.io/en/latest/developing-packages.html#custom-setup-scripts) stanza of your package's `.cabal` config. The `Setup.hs` script looks like:

```haskell
import Language.JavaScript.NodeJS.CabalHook

main :: IO ()
main = defaultMainWithEvalServer pure
```

If you need to add `npm` dependencies, you can add them like this:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.NodeJS.CabalHook

main :: IO ()
main = defaultMainWithEvalServer $ addDependencies [("left-pad", "^1.1.3")]
```

Then, add `nodejs-eval` as a regular dependency. Enable `TemplateHaskell` extension, then you can execute Node.js scripts like this:

```haskell
import Language.JavaScript.NodeJS.Splices

initEvalServer :: IO (T.Text -> IO Value, IO ())
initEvalServer = $(splice)
```

Now, executing `initEvalServer` will start the eval server. Two continuations are returned, the first one accepts a Node.js script, and returns the evaluation result as a `Data.Aeson.Value`. Evaluation failure will raise an exception. The second continuation will terminate the eval server.

See [`nodejs-eval-test`](nodejs-eval-test) for a complete demo.
