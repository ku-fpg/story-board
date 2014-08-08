{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}
module Graphics.Storyboard.Prelude where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text(Text)
import Graphics.Blank as Blank

-- The idea behind the prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in a simulation mode.

newtype Prelude a = Prelude { runPrelude :: Canvas a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: Text -> Text -> Prelude Float
wordWidth font_name txt = Prelude $ saveRestore $ do
    Blank.font $ font_name
    TextMetrics w <- measureText txt
    return w
