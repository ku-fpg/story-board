{-# LANGUAGE KindSignatures, TupleSections, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Act where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Blank

import Graphics.Storyboard.Types

newtype Act a = Act { runAct :: Canvas (a,[NextAct]) }

data NextAct where
  NextAnimationFrame :: Act () -> NextAct

nextAnimationFrame :: Act () -> Act ()
nextAnimationFrame = Act .return . ((),) . (:[]) . NextAnimationFrame

instance MonadCanvas Act where
  liftCanvas m = Act $ fmap (,[]) m

instance Functor Act where
 fmap f m = pure f <*> m

instance Applicative Act where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Act where
  return = Act . return . (,[])
  Act m >>= k = Act $ do
      (a,nx1) <- m
      (r,nx2) <- runAct (k a)
      return (r,nx1 ++ nx2)


instance MonadIO Act where
    liftIO = Act . fmap (,[]) . liftIO
