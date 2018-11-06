# `inline-js-ng`

[![CircleCI](https://circleci.com/gh/tweag/inline-js/tree/master.svg?style=shield)](https://circleci.com/gh/tweag/inline-js/tree/master)

Evolved [`inline-js`](https://github.com/tweag/inline-js). Don't interpret `ng` as "no good"; it's "new generation" :D

Note: The "old generation" of `inline-js` is archived at the [`legacy`](https://github.com/tweag/inline-js/tree/legacy) branch.

## Motivation

Previously, `inline-js` was started as a companion project of [`asterius`](https://github.com/tweag/asterius). Since `asterius` generates code which can be executed in Node.js, we thought it'd be nice to implement some Haskell/Node.js bridge so it could be useful to `asterius` as well as a lot of other projects involved with web stuff. Eventually, `inline-js` didn't graduate from a proof-of-concept, and `asterius` didn't integrate `inline-js`.

The core of the original `inline-js` was essentially an RPC server over HTTP, receiving eval commands from Haskell and returning results. Data exchanging relies on `FromJSON`/`ToJSON` instances of `aeson`. There are certain flaws and also delicate mismatches between what `inline-js` offered and what `asterius` needed:

* `inline-js` doesn't properly perform error handling, especially on the Node.js side. Comprehensive error handling is inherently hard, especially combined with Node.js callbacks, Haskell threads and asynchronous exceptions, but `inline-js` was, esentially, lazy on this one.
* `inline-js` comes with `aeson`, `http-client` and their friends, therefore introduces a huge dependency overhead. For routine web projects it's not a problem at all, but `asterius` seeks to be upstreamed back to GHC, therefore the `asterius` family of packages mustn't rely on anything out of ghc boot libs.
* `inline-js` doesn't have concurrency in mind; it assumes eval commands are dispatched in sequence in a single Haskell thread, and no eval command must be emitted before the previous one returns. This is a serious limitation; we really want to be able to share a session between multiple Haskell threads, and perform evaluations in a thread-safe manner without resorting to using a mutex.
* `inline-js` could only transmit simple JSON-encoded data between Haskell/Node.js. We really want to be able to transmit arbitrary Haskell/Node.js objects.
* `inline-js` required a custom `Setup.hs` for anything depending on it. It's not good. We don't need that degree of compile-time magic.

Several months later, after accumulating enough experience in the development of `asterius` (particularly its JavaScript FFI feature), it looks like a good timing for `inline-js-ng`.

## Features

* [x] Phase 0: Bare bones RPC
    * [x] Lightweight JSON library, including AST/encoder/decoder
    * [x] Lightweight RPC supporting concurrent non-order-preserving requests/responses in a cross platform manner, without relying on HTTP
    * [x] Proper error handling
    * [x] Basic ping/pong unit test powered by QuickCheck generated JSON values
* [x] Phase 1: Eval server
    * [x] Eval server for synchronous code, supporting eval timeouts
    * [x] Eval server for asynchronous code, supporting eval/resolve timeouts
* [ ] Phase 2: Marshaling arbitrary Haskell/Node.js values, including functions
    * [x] Modeling arbitrary Node.js values as `JSRef`s
        * [x] Mapping from `JSRef`s to JavaScript values in the eval server
        * [x] Explicit construction of `JSRef`s based on Haskell eval commands
        * [x] Assembling a Haskell `JSRef` into an eval command
        * [x] Regional destruction of `JSRef`s in Haskell
    * [ ] Modeling arbitrary Haskell values
        * [ ] `Dynamic`-based? More strongly typed? Yet to be explored.
* [ ] Further enhancements:
    * [x] Interaction with Headless Chrome via `puppeteer`
    * [ ] Haskell syntactic sugar, e.g. `foreign import javascript`

For the use case of `asterius`, proceeding to the middle of Phase 2 shall be mostly enough. By then `inline-js-ng` becomes a reliable lightweight Haskell/Node.js bridge which allows `asterius` to run its compiled code and inspect structured debugging data in its test suites.

## Feedback

Currently, this repository is an active laboratory. The API is subject to radical changes based on feedback from the development of `asterius`.

Still, we are interested in how `inline-js-ng` may be useful to your projects. Shall you have any question, suggestion or criticism, please don't hesitate to file an issue!

## Sponsors

[![Tweag I/O](https://www.tweag.io/img/tweag-small.png)](https://www.tweag.io)

`inline-js-ng` is developed by [Tweag I/O](https://tweag.io/).
