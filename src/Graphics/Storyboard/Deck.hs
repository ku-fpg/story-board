{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}
module Graphics.Storyboard.Deck where

import Graphics.Blank(Canvas,DeviceContext,clearRect,send)
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Types
import Graphics.Storyboard.Act
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

data Deck = Deck
  { _deckCavity  :: Cavity Float
  , _deckStack   :: DeckStack
  }

data DeckStack where
  Empty         ::                DeckStack
  DrawOnDeck    :: Act -> Deck -> DeckStack
  PauseDeck     ::        Deck -> DeckStack

drawOnDeck  :: Mosaic () -> Deck -> Deck
drawOnDeck mos deck = Deck
  { _deckCavity = cavity1
  , _deckStack = DrawOnDeck act deck
  } where (act,cavity1) = runMosaic mos cavity0
          cavity0 = deckCavity deck

pause :: Deck -> Deck
pause deck = deck { _deckStack = PauseDeck deck }

deckCavity  :: Deck -> Cavity Float
deckCavity = _deckCavity

defaultDeck :: Size Float -> Deck
defaultDeck sz = Deck
  { _deckCavity = Cavity (0,0) sz
  , _deckStack  = Empty
  }


runDeck :: DeviceContext -> Deck -> IO (Cavity Float)
runDeck context (Deck cavity Empty)      = return cavity
runDeck context (Deck cavity (PauseDeck deck)) = do
    _cavity' <- runDeck context deck
    return cavity
runDeck context (Deck cavity (DrawOnDeck act deck)) = do
    _cavity' <- runDeck context deck
    send context $ runFirstAct act
    return cavity

{-
newtype Deck a = Deck { unDeck :: DeckEnv -> DeckState -> IO (a,DeckState) }

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
  liftIO m = Deck $ \ _ st -> do
    r <- m
    return (r,st)

instance MonadCanvas Deck where
  liftCanvas m = Deck $ \ cxt st -> do
      r <- send (deckContext cxt) m
      return (r,st)

data DeckEnv = DeckEnv
  { deckContext :: DeviceContext
  }

defaultDeckEnv :: DeviceContext -> DeckEnv
defaultDeckEnv cxt = DeckEnv
  { deckContext = cxt
  }

data DeckState = DeckState
  { deckStack   :: [(Cavity Float,Mosaic ())]
  , deckCavity  :: Cavity Float
  }

defaultDeckState :: Size Float -> DeckState
defaultDeckState sz = DeckState
  { deckStack  = []
  , deckCavity = Cavity (0,0) sz
  }

runDeck :: DeviceContext -> Size Float -> Deck a -> IO a
runDeck cxt sz (Deck f) = do
    (r,_) <- f (defaultDeckEnv cxt) (defaultDeckState sz)
    return r

-- Mini-DSL

waitForKey :: Deck ()
waitForKey = return ()  -- for now


-- All comands reflect the internal state on the screen
pushDeck  :: Mosaic () -> Deck (Cavity Float)  -- draw a mosaic onto the deak, in the cavity
pushDeck mos = Deck $ \ env st -> do
  let (act,cav1) = runMosaic mos (deckCavity st)
  -- And print to the screen, please
  send (deckContext env) $ runFirstAct act
  -- And return the new space to work in
  return (cav1,st { deckStack = (deckCavity st,mos) : deckStack st
                  , deckCavity  = cav1
                  })

popDeck  :: Deck (Cavity Float)  -- undraw a mosaic onto the deak.
popDeck = Deck $ \ env st -> do
  case deckStack st of
    [] ->
      return (deckCavity st,st)
    (cav1,_) : ds -> do
      -- clear the cavity space
      case cav1 of Cavity (x,y) (w,h) -> send (deckContext env) $ clearRect (x,y,w,h)
      return (cav1,st { deckStack = ds, deckCavity = cav1 })

-- ???
--cavityOfDeck  :: Deck (Cavity Float)  -- undraw a mosaic onto the deak.



--popDeck   :: Deck ()               -- remove single Mosaic
--resetDeak :: Deck ()              -- remove *all* the Mosaics
-}
