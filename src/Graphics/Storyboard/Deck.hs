{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Deck where

import Graphics.Blank(Canvas)
import Graphics.Storyboard.Types
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class


newtype Deck a = Deck { unDeck :: DeckState -> Canvas (a,DeckState) }

instance Functor Deck where
 fmap f m = pure f <*> m

instance Applicative Deck where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Deck where
  return a = Deck $ \ st -> return (a,st)
  Deck m >>= k = Deck $ \ st0 -> do
    (r,st1) <- m st0
    unDeck (k r) st1

instance MonadIO Deck where
    liftIO = liftCanvas . liftIO

instance MonadCanvas Deck where
  liftCanvas m = Deck $ \ st -> do
      r <- m
      return (r,st)

data DeckState = DeckState {}

defaultDeckState :: DeckState
defaultDeckState = DeckState {}


-- All comands reflect the internal state on the screen
--pushDeck  :: Mosaic -> Deak ()    -- draw a mosaic onto the deak, in the cavity
--popDeck   :: Deck ()               -- remove single Mosaic
--resetDeak :: Deck ()              -- remove *all* the Mosaics

runDeck :: Deck a -> Canvas a
runDeck (Deck f) = do
    (r,_) <- f defaultDeckState
    return r

{-
deak         :: Deak                      -- empty, nothing, none
transparency :: Mosaic -> Deck -> Deck    -- place virtual slide on deak

drawDeak :: Deak -> Canvas Deak       -- Remember what you have drawn

pushMosaic :: Mosaic -> Deck -> Canvas Deck   -- draw a mosaic onto the deak, in the cavity
popMosaic  :: Deck           -> Canvas Deck   -- remove a Foil
-}
