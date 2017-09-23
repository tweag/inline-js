{-# OPTIONS_GHC -Wall #-}

import Language.JavaScript.Inline.Configure

main :: IO ()
main = defaultMainWithInlineJS defaultConfigureOptions
