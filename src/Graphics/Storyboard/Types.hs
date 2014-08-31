{-# LANGUAGE KindSignatures, RankNTypes, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Types where

import Data.Text(Text)
import Graphics.Blank(Canvas)
import Graphics.Blank.Style(Style)

type Size  f = (f,f)
type Coord f = (f,f)

type Color = Text

class Monad m => MonadCanvas m where
  liftCanvas :: Canvas a -> m a

instance MonadCanvas Canvas where
  liftCanvas m = m

data Background = forall style .Style style => Background style

instance Show Background where
  show (Background {}) = "<background>"

bgColor :: Text -> Background
bgColor = Background

class Drawing picture where
   drawCanvas :: Size Float -> picture -> Canvas ()
