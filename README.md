# `inline-js`

[![CircleCI](https://circleci.com/gh/tweag/inline-js/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/inline-js/tree/master)

## Introduction

This repository contains two packages, `inline-js-core` and `inline-js`.

`inline-js-core` implements the logic to manage a `node` session and send/receive messages to run JavaScript. It only depends on ghc's boot libraries.

`inline-js` follows in the tradition of [inline-c](http://hackage.haskell.org/package/inline-c) and [inline-java](http://hackage.haskell.org/package/inline-java) in enabling developers to bridge the gap between Haskell and another programming language. It comes with two QuasiQuoters, `expr` and `block`, to allow embedding a JavaScript expression/block in Haskell. It relies on `FromJSON`/`ToJSON` classes of `aeson` for Haskell/JavaScript data conversion.

Also check our [blog post](https://www.tweag.io/posts/2019-05-09-inline-js.html) for introductory examples and how it works under the hood.

## Quick examples

### A sample use of `expr`:

``` haskell
sumInJS :: Int -> Int -> IO Int
sumInJS v1 v2 =
    withJSSession defJSSessionOpts [expr| $v1 + $v2 |]
```

To antiquote a Haskell variable into the JavaScript context, just precede its name with `$`. The two `Int` values can be inserted since `Int` is already an instance of `ToJSON` and `FromJSON`.

Note that there is an implicit `return` inserted when using `expr`, so don't add one yourself. If you need to do more complex work like using `for`-loops, you'll want to use `block`.

### A sample use of `block`:

``` haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

data GameWorld = GameWorld
  { playerCharacter :: PlayerCharacter
  } deriving (Generic, ToJSON, FromJSON)

data PlayerCharacter = PlayerCharacter
  { hp :: Int
  , maxHp :: Int
  , isDead :: Bool
  } deriving (Generic, ToJSON, FromJSON)

getCharacterStatus :: IO String
getCharacterStatus =
    let gameWorld =
            GameWorld
              { playerCharacter =
                  PlayerCharacter {hp = 10, maxHp = 30, isDead = False}
              }
    in
        withJSSession defJSSessionOpts [block|
            const gameWorld = $gameWorld;
            const player = gameWorld.playerCharacter;
            const renderHp = somePlayer => somePlayer.hp + "/" + somePlayer.maxHp;
            if (player.hp <= 0 || player.isDead) {
                return "Game Over";
            } else {
                return "Your HP: " + renderHp(player);
            }
        |]
```

Here, you can see the Haskell value `gameWorld` (which has both `ToJSON` and `FromJSON` derived) serialized into the JavaScript quotation with the antiquotation `$gameWorld`. Explicit use of `return` is required to extract a value out from the quotation.

Note that both `expr`/`block` supports using `await`, making it convenient to work with asynchronous JavaScript APIs based on `Promise`s.

### Beyond passing JSON data

The `expr`/`block` QuasiQuoters in `inline-js` pass JSON data between Haskell/JavaScript. `inline-js-core` additionally provides ability to:

* Pass raw binary data, or `JSVal`s which are references of arbitrary JavaScript values.
* Run dynamic `import()` to import a built-in module, npm module in `node_modules`, or a `.mjs` module file.
* Specify evaluate/resolve timeouts when evaluating JavaScript.
* Decouple the send/receive processes, so it's possible to asynchronously send a batch of requests and later retrieve the responses.
* Exporting a Haskell function as a JavaScript function. It's even possible to make the JavaScript function *synchronous* so it can be used as a WebAssembly import!
* Manipulate `stdin`/`stdout`/`stderr` handles of the underlying `node` process.

See the haddock documentation of `Language.JavaScript.Inline.Core` for details. If you're using `inline-js`, `Language.JavaScript.Inline` also re-exports that module.

### Session Management

`inline-js` exposes the `withJSSession`, `newJSSession` and `closeJSSession` functions, with options for configuring the session. `inline-js` works by communicating with a Node.js "eval server" script which evaluates received JavaScript code snippets.

To avoid polluting the global namespace, the high-level `expr`/`block` QuasiQuoters wrap JavaScript code in the IIFE(Immediately Invoked Function Expression) style, so `let`, `var` and `const` variable declarations in the splices are not shared even within the same session. This is not the case if you're using lower-level `eval` from `inline-js-core` instead.

Important note: do not run untrusted JavaScript via `inline-js-core`/`inline-js`. They don't provide any kind of security guarantee.

## Building

Simply `stack build` shall do. Run `stack test inline-js` to run the test suite, `stack test inline-js --test-arguments="-j8"` for parallel testing. `cabal new-build` should also work.

`inline-js-core`/`inline-js` requires at least nodejs 11 to work; earlier versions will be rejected upon initialization of `JSSession`.

## Sponsors

[![Tweag I/O](https://www.tweag.io/img/tweag-small.png)](https://www.tweag.io)

`inline-js-core`/`inline-js` are developed by [Tweag I/O](https://tweag.io/).
