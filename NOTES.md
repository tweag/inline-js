# inline-js implementation notes

## Rationale

Asterius implements Template Haskell in a similar manner to GHCJS: running
compiled code in a `node` process. Simplest way to do it would be calling `node`
with each TH splice evaluation, passing input/output via `stdin` / `stdout`,but
we thought that an `inline-*` style library that enables more principled
interaction with the JavaScript world is a nice yak to shave, hence the creation
of `inline-js`.

The primary user of `inline-js` is Asterius, which will be refactored as a
proper GHC cross compilation backend in the future. This restricts our technical
choices:

- To link against `node`, or perform IPC? The former has much higher efficiency,
  and makes it easier for Haskell & JavaScript heap to coordinate, but will
  significantly complicate the build system of `inline-js`. So we favor IPC
  here.
- How is IPC performed? Over sockets or what? Do we conform to certain
  standards, like protobuf or anything like that? Since we need to guarantee
  simplicity of implementation, we can't use a well-established IPC standard
  here, since they often involve code generation and further complicates the
  build system. We only use `stdin` / `stdout` of the `node` process, since it's
  the only portable way that works with all major platforms; a very simple
  custom binary serialization protocol is used, since the messages to be
  exchanged is fully known beforehand, we don't need to address some common
  issues related to IPC messages.
- What Haskell dependencies can we bring in? They'll become "boot libs" of GHC,
  so `aeson` and friends would increase dependency footprint of Asterius, making
  it much harder to be merged back to GHC HQ. So we split the project into two
  layers: the `inline-js-core` package, which only depends on existing GHC boot
  libs, have basic `node` interop logic to be used by Asterius, and the
  `inline-js` package which re-exports `inline-js-core`, while adding `aeson`
  support and is intended to be used by non-Asterius Haskell projects.
- How to lex/parse JavaScript? This is needed when implementing the TH
  quasiquoters in `inline-js`. We used to use `language-javascript`, but
  JavaScript is a complex and evolving language, and the Haskell parser has a
  lot of rough edges. So now we use a JavaScript parser which is already
  battle-tested in the JavaScript ecosystem; there's plenty to choose from, and
  our current choice is `acorn`.

## Package structure

- `inline-js-core`: ships a hand-written JavaScript file to be executed by
  `node`, and contains `Language.JavaScript.Inline.Core*` which enables `node`
  interop
- `inline-js`: ships a compiled JavaScript file which implements parsing logic,
  and contains `Language.JavaScript.Inline` to be used by regular projects
- `inline-js-examples`: they're supposed to be self-documenting modules to
  demonstrate various ways of using `inline-js`
- `inline-js-tests`: a unified test suite implementing all unit tests for the
  project.

## The nodejs eval server

`inline-js-core/jsbits/index.js` is a self-contained JavaScript file to be run
by `node` when a `Session` starts. It parses messages from `stdin`, handles
them, and when reply messages are generated (there isn't a 1-to-1 mapping from
input to output messages!), the messages are written to `stdout`. It exits the
process when a shutdown message is received.

The message format is defined at `Language.JavaScript.Inline.Core.Message`. The
`MessageHS` type is sent to `node` from Haskell, and `MessageJS` the other way.
The primary `MessageHS` request variants are:

- `JSEvalRequest`: we need to run some JavaScript code, so send the "code"
  (we'll explain what "code" means later) tagged with a unique id and its type
  information.
- `JSValFree`: when we detect that a `JSVal` (Haskell reference to a JavaScript
  value) is unreachable, we send this to inform the eval server to drop its
  reference on the JavaScript side as well. This doesn't necessarily result in
  removal of that value on the JavaScript heap though, since it may be
  referenced elsewhere.
- `HSExportRequest`: we support exporting Haskell functions to the JavaScript
  world, and they appear as ordinary JavaScript functions on the JavaScript
  side! `JSVal`s are normally eval results of `JSEvalRequest`, but reference to
  an exported Haskell function needs special handling, hence this request.
- `HSEvalResponse`: the JavaScript world may attempt to call Haskell functions,
  which will send `HSEvalRequest` messages back (it's a `MessageJS` variant).
  When Haskell evaluation completes, this message will be sent back to the
  JavaScript world.

## The type system

In `Language.JavaScript.Inline.Core.Class`, we define two type classes:

- `ToJS`: whatever that can be sent to the JavaScript world, be it a raw buffer,
  a string, a JSON value, or a `JSVal`, they are `ToJS` instances.
- `FromJS`: whatever that can be sent from the JavaScript world to Haskell, is a
  `FromJS` instance.

`ToJS` is fairly simple: it marshals the input to a `JSExpr`. Conceptually, the
`JSExpr` is a JavaScript expression that can be concated, and it is the
`JSEvalRequest` payload. It's not simply a UTF-8 string though, since we want to
efficiently embed raw buffers and other stuff, so `JSExpr` is a list of
`JSExprSegment` which can be different things; the `toJS()` method in the eval
server script will perform decoding of the serialized `JSExpr`.

Before we talk about `FromJS`, we need to classify what kinds of info we want to
retrieve from the JavaScript world as eval results:

- Values: buffers, strings, JSONs, etc. These things can be fully serialized to
  pass back, no need to care about lifetime issues or whatever. As a special
  case, `undefined` or `()` also belongs to this category, which means only
  execute side effects on the JavaScript side and don't pass anything back.
- References: it can be an arbitrary JavaScript value, so it may reference
  something which can't be serialized, like handles to system resources. They
  need special handling:
    - The eval server maintains a mapping from integers to JavaScript values.
      The key is the unique identifier to be sent between Haskell/JavaScript.
      Since the JavaScript side doesn't know about the Haskell heap, it has to
      explicitly maintain these references unless explicitly told to drop one.
    - The Haskell side has a special `JSVal` type which models these references.
      They have attached finalizers that sends `JSValFree` messages back when
      being garbage collected.

`Language.JavaScript.Inline.Core.Message` implements `RawJSType` which
classifies different kinds of JavaScript things, and it is used by the `FromJS`
class. The eval server script also uses this simple type system to decide how to
decode/encode things.

## Evaluating JavaScript from Haskell

What happens when we evaluate a piece of JavaScript?

- Given a `Session`, we can `eval` a `JSExpr` (remember the `ToJS` class yet?)
  It'll create the `JSEvalRequest` payload, assign it with a unique id, and send
  it to `node`.
- In the eval server, the `MainContext` class runs in the main thread, and the
  `recvLoop` method will intercept the message, then redirect it to the worker.
- The `WorkerContext` class runs in a worker thread, and the parent thread sends
  it a message, to be handled by the `handleParentMessage` method.
- The evaluation may be asynchronous; so we attach handlers to the `Promise`
  that represents eval result, and when eval completes either successfully or
  with error, the handlers will construct the reply message, to be sent back to
  the main thread, then sent to `stdout` of the `node` process.
- On the Haskell side, there's a forked thread which polls the `stdout` of the
  `node` process and when it's `JSEvalResponse`, the response message will
  contain the unique id of the eval request. The id is in fact a `StablePtr` of
  a Haskell `TMVar`, and it's used to get the `TMVar` back and fulfill it. The
  `eval` call in the beginning returns a `IO r` continuation, which attempts to
  fetch result from that `TMVar`, and it'll block when the result isn't yet
  available.
- The `IO r` continuation uses lazy I/O; the returned `r` value is a thunk and
  the blocking behavior happens only when forcing the thunk. Normally lazy I/O
  is an antipattern, but here it allows us to utilize JavaScript concurrency
  while simplifying the eval API:
    - We want to be able to issue a bunch of eval requests first, and collect
      the results later, without needing to fork a Haskell thread for each
      request.
    - If we don't use laziness here, we need two distinct APIs: one for
      returning the actual result value, another one for returning a handle for
      later retrieval of the result.
- It's possible that evaluation will fail, resulting in a rejected `Promise`. In
  that case, the serialized JavaScript error message is sent back, and the `IO
  r` continuation will rethrow it as a Haskell exception.
- It's also possible that a critical error happens at the `node` side. We do our
  best effort to send a `FatalError` message back, and the Haskell receive
  thread will fulfill a special `TMVar` with the fatal error. All pending `IO r`
  continuations will throw, since their STM computation will first attempt to
  poll the fatal error inbox, and then the individual message inbox.

## Evaluating Haskell from JavaScript

What happens when we attempt to export Haskell functions to JavaScript:

- Given a `Session`, we can `export` a Haskell function and return the
  corresponding `JSVal`, if the function type is a `Export` instance. Users
  shouldn't handwrite `Export` instances; for the function type `a -> b -> .. ->
  IO r`, if arguments `a`, `b`, etc are `FromJS` instances, and `r` is `ToJS`
  instance, then it can be exported.
- An `HSExportRequest` is constructed and sent to the `node` side. In addition
  to the id associated with the request, there's also an id associated with the
  exported Haskell function, which will be used by the JavaScript side as a
  reference to the Haskell heap.
- The eval server receives the request. It uses the type information in the
  request to generate a JavaScript function that, when called, will send
  `HSEvalRequest` back to Haskell and waits for Haskell's eval response. The
  function is then encapsulated as a `JSVal` to be sent back to Haskell.

What happens when that JavaScript function is called:

- The Haskell function can be exported as either an async or sync JavaScript
  function. The async way is recommended. When the JavaScript function is
  called, it queries the previously cached type information to decide how to
  marshal the arguments and construct the `HSEvalRequest` payload. The request
  is sent, and the function returns a `Promise` which fulfills or rejects when
  the corresponding `HSEvalResponse` is sent back.
- In some cases, you really really want the JavaScript function to be a sync
  one: for instance, WebAssembly import functions can't be async. In that case,
  the JavaScript function will block until the result is available. The blocking
  mechanism is implemented by shared buffers and atomics, and that's the only
  reason why we need the main/worker thread distinction in the eval server. It's
  a very heavy hammer, slow and non-reentrant.
