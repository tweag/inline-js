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
    InlineJSException (..),
    eval,
    alloc,
    importMJS,
    eval',
    alloc',
    importMJS',
  )
where

import Language.JavaScript.Inline.Core.Command
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.JSCode hiding
  ( importMJS,
  )
import Language.JavaScript.Inline.Core.Session
