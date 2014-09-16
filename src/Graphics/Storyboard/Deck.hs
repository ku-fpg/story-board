{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Deck where

import Graphics.Blank(Canvas,DeviceContext)
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Types
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class


newtype Deck a = Deck { unDeck :: DeckEnv -> DeckState -> Canvas (a,DeckState) }

instance Functor Deck where
 fmap f m = pure f <*> m

instance Applicative Deck where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Deck where
  return a = Deck $ \ _ st -> return (a,st)
  Deck m >>= k = Deck $ \ env st0 -> do
    (r,st1) <- m env st0
    unDeck (k r) env st1

instance MonadIO Deck where
    liftIO = liftCanvas . liftIO

instance MonadCanvas Deck where
  liftCanvas m = Deck $ \ _ st -> do
      r <- m
      return (r,st)

data DeckEnv = DeckEnv
  { deckContext :: DeviceContext
  }

defaultDeckEnv :: DeviceContext -> DeckEnv
defaultDeckEnv cxt = DeckEnv
  { deckContext = cxt
  }

data DeckState = DeckState
  { deckMosaics :: [Mosaic ()]
  , deckCavity  :: Cavity Float
  }

defaultDeckState :: Size Float -> DeckState
defaultDeckState sz = DeckState
  { deckMosaics = []
  , deckCavity = Cavity (0,0) sz
  }

runDeck :: DeviceContext -> Size Float -> Deck a -> Canvas a
runDeck cxt sz (Deck f) = do
    (r,_) <- f (defaultDeckEnv cxt) (defaultDeckState sz)
    return r

-- Mini-DSL

waitForKey :: Deck ()
waitForKey = return ()  -- for now

-- All comands reflect the internal state on the screen
pushDeck  :: Mosaic () -> Deck ()    -- draw a mosaic onto the deak, in the cavity
pushDeck mos = Deck $ \ env st -> do
  let (act,cav1) = runMosaic mos (deckCavity st)
  -- And print to the screen, please
  -- And return 
  return ((),st { deckMosaics = mos : deckMosaics st
                , deckCavity  = cav1
                })

--popDeck   :: Deck ()               -- remove single Mosaic
--resetDeak :: Deck ()              -- remove *all* the Mosaics
