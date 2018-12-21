{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline
  ( js
  ) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))

js :: QuasiQuoter
js =
  QuasiQuoter
    { quoteExp = error "Language.Java.Inline: quoteExp"
    , quotePat = error "Language.Java.Inline: quotePat"
    , quoteType = error "Language.Java.Inline: quoteType"
    , quoteDec = error "Language.Java.Inline: quoteDec"
    }
