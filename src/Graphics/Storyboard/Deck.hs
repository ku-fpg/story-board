{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}
module Graphics.Storyboard.Deck where

import Control.Concurrent.STM

import Data.Text (Text)
import Data.Time.Clock

import Graphics.Blank (DeviceContext, clearRect, send, eventQueue, eType, eWhich)
import Graphics.Storyboard.Act
import Graphics.Storyboard.Behavior
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Types

------------------------------------------------------------------------

data Deck = Deck
  { _deckCavity  :: Cavity Double
  , _deckStack   :: DeckStack
  }

instance Show Deck where
  show (Deck cav stk) = show cav ++ ":" ++ show stk

data DeckStack where
  Empty         ::                DeckStack
  DrawOnDeck    :: Act -> Deck -> DeckStack
  PauseDeck     ::        Deck -> DeckStack

instance Show DeckStack where
  show Empty = "Empty"
  show (DrawOnDeck _ deck) = "Draw :" ++ show deck
  show (PauseDeck deck)    = "Pause :" ++ show deck

drawOnDeck  :: Mosaic () -> Deck -> Deck
drawOnDeck mos deck = Deck
  { _deckCavity = cavity1
  , _deckStack = DrawOnDeck act deck
  } where (act,cavity1) = runMosaic mos cavity0
          cavity0 = deckCavity deck

------------------------------------------------------------------------

-- Our mini-DSL

pauseDeck :: Deck -> Deck
pauseDeck deck = deck { _deckStack = PauseDeck deck }

deckCavity  :: Deck -> Cavity Double
deckCavity = _deckCavity

defaultDeck :: Size Double -> Deck
defaultDeck sz = Deck
  { _deckCavity = Cavity (0,0) sz
  , _deckStack  = Empty
  }

------------------------------------------------------------------------

-- This is the heart of story-board, the run function
-- for the Deck.

runDeck :: DeviceContext -> Deck -> IO UserEvent
runDeck context deck = do
  r <- runDeck' context deck
  case r of
    Left msg -> return msg
    Right _ -> return ForwardSlide

runDeck' :: DeviceContext -> Deck -> IO (Either UserEvent (Cavity Double))
runDeck' context (Deck cavity Empty)      = do
  case cavity of Cavity (x,y) (w,h) -> send context $ clearRect (x,y,w,h)
  return (Right cavity)
runDeck' context (Deck cavity (PauseDeck deck)) = runDecking context deck $ \ _ -> do
    let loop = do
          event <- readTChan (eventQueue context)
          if eType event == "keypress"
          then return event
          else loop -- ignore other things, for now
    event <- atomically $ loop
    print ("got key" :: Text, event)
    case eWhich event of
      Just 98 -> return (Left BackSlide)
      _       -> return (Right cavity)
runDeck' context (Deck cavity (DrawOnDeck act deck)) = runDecking context deck $ \ cavity' -> do

    start_tm <- getCurrentTime

    let loop behEnv0 clr = do
          -- First, animate the frame
          theAct <- atomically $ runAct behEnv0 act
          print cavity
          done <- send context $ do
                  -- We can do better, we need the different between the two cavities
                    if clr then case cavity' of Cavity (x,y) (w,h) -> clearRect (x,y,w,h)
                           else return ()
                    theAct
          if done
          then return ()
          else do -- next figure out the events
                  d <- registerDelay (10 * 1000)
                  let ev = do
                         event <- readTChan (eventQueue context)
                         return (Just event)
                  let pz = do
                          b <- readTVar d
                          if b then return Nothing
                               else retry

                  rz <- atomically $ ev `orElse` pz

                  -- next, reset the env
                  tm1 <- getCurrentTime
                  let diff :: Double = realToFrac (diffUTCTime tm1 start_tm)
                  let behEnv1 = nextBehaviorEnv diff rz behEnv0

                  case rz of
                    Just event | eType event == "keypress" ->
                        do atomically $ unGetTChan (eventQueue context) event
                           return ()
                    _ -> loop behEnv1 True


    loop defaultBehaviorEnv False

--    send context $ runFirstAct act
    return (Right cavity)

data UserEvent
  = ForwardSlide
  | BackSlide

runDecking :: DeviceContext -> Deck -> (Cavity Double -> IO (Either UserEvent b)) -> IO (Either UserEvent b)
runDecking context deck k = do
  r <- runDeck' context deck
  case r of
    Left msg -> return (Left msg)
    Right cavity -> k cavity



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
  { deckStack   :: [(Cavity Double,Mosaic ())]
  , deckCavity  :: Cavity Double
  }

defaultDeckState :: Size Double -> DeckState
defaultDeckState sz = DeckState
  { deckStack  = []
  , deckCavity = Cavity (0,0) sz
  }

runDeck :: DeviceContext -> Size Double -> Deck a -> IO a
runDeck cxt sz (Deck f) = do
    (r,_) <- f (defaultDeckEnv cxt) (defaultDeckState sz)
    return r

-- Mini-DSL

waitForKey :: Deck ()
waitForKey = return ()  -- for now


-- All comands reflect the internal state on the screen
pushDeck  :: Mosaic () -> Deck (Cavity Double)  -- draw a mosaic onto the deak, in the cavity
pushDeck mos = Deck $ \ env st -> do
  let (act,cav1) = runMosaic mos (deckCavity st)
  -- And print to the screen, please
  send (deckContext env) $ runFirstAct act
  -- And return the new space to work in
  return (cav1,st { deckStack = (deckCavity st,mos) : deckStack st
                  , deckCavity  = cav1
                  })

popDeck  :: Deck (Cavity Double)  -- undraw a mosaic onto the deak.
popDeck = Deck $ \ env st -> do
  case deckStack st of
    [] ->
      return (deckCavity st,st)
    (cav1,_) : ds -> do
      -- clear the cavity space
      case cav1 of Cavity (x,y) (w,h) -> send (deckContext env) $ clearRect (x,y,w,h)
      return (cav1,st { deckStack = ds, deckCavity = cav1 })

-- ???
--cavityOfDeck  :: Deck (Cavity Double)  -- undraw a mosaic onto the deak.



--popDeck   :: Deck ()               -- remove single Mosaic
--resetDeak :: Deck ()              -- remove *all* the Mosaics
-}
