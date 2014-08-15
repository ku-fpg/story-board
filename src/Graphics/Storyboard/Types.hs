{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Types where

import Graphics.Blank(Canvas)

type Size  f = (f,f)
type Coord f = (f,f)

class Monad m => MonadCanvas m where
  liftCanvas :: Canvas a -> m a

instance MonadCanvas Canvas where
  liftCanvas m = m
