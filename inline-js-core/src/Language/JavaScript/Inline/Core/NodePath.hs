{-# LANGUAGE CPP #-}

module Language.JavaScript.Inline.Core.NodePath
  ( defNodePath,
  )
where

{-# INLINE defNodePath #-}
defNodePath :: FilePath
#if defined(INLINE_JS_NODE)
defNodePath = INLINE_JS_NODE
#else
defNodePath = "node"
#endif
