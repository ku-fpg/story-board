{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}
module Graphics.Storyboard.Prelude where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text(Text)
import Graphics.Blank as Blank

import Graphics.Storyboard.Prose


-- The idea behind the prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in a simulation mode.

newtype Prelude a = Prelude { runPrelude :: Canvas a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: TheProseStyle -> Text -> Prelude Float
wordWidth cxt txt = Prelude $ saveRestore $ do
    Blank.font $ fontName cxt
    TextMetrics w <- measureText txt
    return w
