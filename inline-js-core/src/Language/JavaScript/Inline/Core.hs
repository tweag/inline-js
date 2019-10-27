module Language.JavaScript.Inline.Core
  ( JSSessionOpts (..),
    defJSSessionOpts,
    JSSession,
    newJSSession,
    closeJSSession,
    withJSSession,
    nodeStdIn,
    nodeStdOut,
    nodeStdErr,
    JSCode (..),
    bufferToString,
    jsonParse,
    jsonStringify,
    JSVal (..),
    deRefJSVal,
    freeJSVal,
    takeJSVal,
    HSFunc (..),
    EvalException (..),
    eval,
    evalWithTimeout,
    alloc,
    importMJS,
    exportHSFunc,
    exportSyncHSFunc,
    eval',
    evalWithTimeout',
    alloc',
    importMJS',
    exportHSFunc',
    exportSyncHSFunc',
  )
where

import Language.JavaScript.Inline.Core.Command
import Language.JavaScript.Inline.Core.HSCode
import Language.JavaScript.Inline.Core.JSCode hiding
  ( importMJS,
  )
import Language.JavaScript.Inline.Core.Session
