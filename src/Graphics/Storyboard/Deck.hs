{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Deck where

import Graphics.Blank(Canvas,DeviceContext)
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Tile
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


data DeckEnv = DeckEnv
  {
  }

data DeckState = DeckState
  { deckMosaics :: [Mosaic ()]
  , deckCavity  :: Cavity Float
  , deckContext :: DeviceContext
  }

defaultDeckState :: DeviceContext -> Size Float -> DeckState
defaultDeckState cxt sz = DeckState
  { deckMosaics = []
  , deckCavity = sz
  , deckContext = cxt
  }

runDeck :: DeviceContext -> Size Float -> Deck a -> Canvas a
runDeck cxt sz (Deck f) = do
    (r,_) <- f $ defaultDeckState cxt sz
    return r

-- Mini-DSL

waitForKey :: Deck ()
waitForKey = return ()  -- for now

-- All comands reflect the internal state on the screen
pushDeck  :: Mosaic () -> Deck ()    -- draw a mosaic onto the deak, in the cavity
pushDeck mos = Deck $ \ st -> do
  let Tile (_,_) m = pack mos
  let act = m (deckOffset st) (deckCavity st)
  let cavity = spacingInMosaic mos (deckCavity st)
  -- And print to the screen, please
  return ((),st { deckMosaics = mos : deckMosaics st
                , deckOffset = cavityCorner cavity
                , deckSize   = cavitySize cavity
                })

--popDeck   :: Deck ()               -- remove single Mosaic
--resetDeak :: Deck ()              -- remove *all* the Mosaics
