{-# LANGUAGE CPP #-}

module NPMPath
  ( defNPMPath,
  )
where

defNPMPath :: FilePath
#if defined(INLINE_JS_NPM)
defNPMPath = INLINE_JS_NPM
#else
defNPMPath = "npm"
#endif
