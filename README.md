# `inline-js`: Call JavaScript from Haskell

![](https://github.com/tweag/inline-js/workflows/pipeline/badge.svg?branch=master)
[![Gitter](https://img.shields.io/gitter/room/tweag/inline-js)](https://gitter.im/tweag/inline-js)

## Implemented features

### `inline-js-core`

- Manage `node` sessions which run the eval server script for Haskell/JavaScript
  interop
- Evaluate expressions with `require()`/`import()` support
- Load third party libraries in user-specified `node_modules`
- Garbage-collected `JSVal` references in Haskell
- Support `Promise`-based async evaluation
- Doesn't require `node` native addon or third party libraries
- Only uses GHC boot libs

## Planned features

### `inline-js-core`

- Export Haskell functions as async/sync JavaScript functions

### `inline-js`

- Type classes for Haskell/JavaScript data marshaling, with `aeson` support
- TH quasi-quoters for lifting Haskell bindings into JavaScript code
- Integrate with TypeScript compiler, generate Haskell code from TypeScript
  `.d.ts` code
- Integrate with headless browser testing frameworks like
  `playwright`/`puppeteer` for running JavaScript in browsers

## Supported versions

Supported GHC versions:

- `ghc-8.8`, tested with `ghc-8.8.3`
- `ghc-8.10`, tested with `ghc-8.10.1`

Supported platforms:

- Windows 10 x64, starting from `ghc-8.10`, tested with Windows Server 2019
- Linux x64, tested with Ubuntu bionic and Alpine edge
- macOS x64, tested with Catalina

Supported `node` versions:

- `node-v12`, minimum `v12.17.0`
- `node-v13`, minimum `v13.14.0`
- `node-v14`, minimum `v14.0.0`

See the [CI
config](https://github.com/tweag/inline-js/blob/master/.github/workflows/pipeline.yml)
for details.

## Documentation

[Haddock documentation](https://inline-js.netlify.app) for HEAD is available.

## Contributors

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; [<img
src="https://tweag.io/logo.png" height="65">](https://tweag.io)

`inline-js` is maintained by [Tweag I/O](https://tweag.io/).

Have questions? Need help? Tweet at [@tweagio](https://twitter.com/tweagio).
