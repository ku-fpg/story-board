{-# LANGUAGE KindSignatures, TupleSections, GADTs,
     GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Act where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank


import Graphics.Storyboard.Types

--newtype Act = Act { runAct :: [Action] }

data Act where
    -- | the bool signifies the finality of the drawing; False = more to draw
  Act     :: Canvas ()                     -> Act
  OnEvent :: Behavior a -> (a -> Canvas Bool) -> Act
  Listen  :: STM ()                         -> Act

  Acts    :: Act -> Act                    -> Act
  NoAct   ::                                   Act

-- return True if you are finished.
--animation :: Canvas Bool -> Act
--animation = Action

-- TODO: change to act
action :: Canvas () -> Act
action = Act

actOnBehavior :: Behavior a -> (a -> Canvas Bool) -> Act
actOnBehavior = OnEvent

listen :: STM () -> Act
listen = Listen

--onEvent :: Canvas a -> Act (a -> Act a)

--  Listen       :: STM a -> Event a               -> Action

runFirstAct :: Act -> Canvas ()
runFirstAct (Act m)      = m
runFirstAct (OnEvent _ k) = return ()
runFirstAct (Listen m) = return ()
runFirstAct (Acts a1 a2) = do
  runFirstAct a1
  runFirstAct a2
runFirstAct NoAct = return ()


-- run the Act; return True if you are finished
-- Still considering threading time through here.
-- it will allow a isClocked :: Act -> Bool function.
runAct :: TheBehaviorEnv -> Act -> Canvas Bool
runAct env (Act m)         = return True
runAct env (OnEvent beh k) = do
      t <- liftIO $ atomically $ evalBehavior env beh
      k t
runAct env (Listen m) = return True
runAct env (Acts a1 a2) = do
  r1 <- runAct env a1
  r2 <- runAct env a2
  return $ r1 && r2
runAct env NoAct = return True


-- This STM only succeeds if one of STM succeeded
runListen :: Act -> STM ()
runListen (Act m) = return ()
runListen (OnEvent _ k) = return ()
runListen (Listen m) = m
runListen (Acts a1 a2) = runListen a1 `orElse` runListen a2
runListen NoAct = return ()

--actAct ::

--replay       :: Float -> (Float -> Canvas ()) -> Act
--replay dur k = Replay dur k

done :: Float -> Act
done = undefined

--listen :: STM a -> Queue a -> Act          -- listen for mouse or keyboard
--listen stm q = Act $ (:[]) $  Listen stm q

instance Semigroup Act where
  (<>) = Acts

instance Monoid Act where
  mempty = NoAct
  mappend = Acts


-----------------------------------------------------------------


-- The assumption is that the history timestamps are the same
-- as the main timestamp.

data TheBehaviorEnv = TheBehaviorEnv
  { theTimer  :: Historic Float
  , theEvent  :: Historic (Maybe Blank.Event)
  , theTimestamp :: Timestamp
  }

defaultBehaviorEnv :: TheBehaviorEnv
defaultBehaviorEnv = TheBehaviorEnv
  { theTimer  = (0,0,0)
  , theEvent  = (Nothing,0,Nothing)
  , theTimestamp = 0
  }

nextBehaviorEnv :: Float -> Maybe Blank.Event -> TheBehaviorEnv -> TheBehaviorEnv
nextBehaviorEnv t e env = TheBehaviorEnv
  { theTimer  = consHistoric t $ theTimer env
  , theEvent  = consHistoric e $ theEvent env
  , theTimestamp = theTimestamp env + 1
  }

type Timestamp = Int
type Historic a = (a,Timestamp,a)

data Behavior :: * -> * where
  Behavior :: TVar (a,Timestamp,a)
           -> (TheBehaviorEnv -> STM a)
           -> Behavior a
  PureB     :: a -> Behavior a
  AppB      :: Behavior (a -> b) -> Behavior a -> Behavior b
  TimerB    :: Behavior Float
  EventB    :: Behavior (Maybe Blank.Event)

timerB    :: Behavior Float
timerB = TimerB

eventB    :: Behavior (Maybe Blank.Event)
eventB = EventB

evalBehavior ::TheBehaviorEnv -> Behavior a -> STM a
evalBehavior env (Behavior var fn) = do
  history@(new,clk,old) <- readTVar var
  if clk + 1 == theTimestamp env
  then do
    newest <- fn env
    writeTVar var (newest,clk + 1,new)
    return newest
  else return $ evalHistoric env history

evalBehavior env (PureB a) = return a
evalBehavior env (AppB f a) = evalBehavior env f <*> evalBehavior env a
evalBehavior env TimerB = return $ evalHistoric env (theTimer env)
evalBehavior env EventB = return $ evalHistoric env (theEvent env)

evalHistoric :: TheBehaviorEnv -> Historic a -> a
evalHistoric env (new,clk,old)
    | clk - 1 == theTimestamp env = old
    | clk     == theTimestamp env = new
    | otherwise                   = error "not enough history for behaviour"

consHistoric :: a -> Historic a -> Historic a
consHistoric a2 (a1,t,a0) = (a2,t+1,a1)

instance Functor Behavior where
  fmap f b = pure f <*> b

instance Applicative Behavior where
  pure = PureB
  (<*>) = AppB

switch :: (a -> b -> b) -> b -> Behavior a -> IO (Behavior b)
switch f b bah = do
  var <- newTVarIO (b,0,b)
  let ret = Behavior var $ \ env -> do
        a <- evalBehavior env bah
        -- look back a bit in time, to see what the value was
        b <- evalBehavior env { theTimestamp = theTimestamp env - 1 } ret
        return $ f a b
  return $ ret

--  Behavior :: TVar a -> Behavior a
{-
  TimerB    :: Behavior Float
  EventB    :: Behavior Blank.Event
  Pure      :: a -> Behavior a
  Ap        :: Behavior (a -> b) -> Behavior a -> Behavior b
--  Now   ::              Event Float

switch :: (a -> b -> b) -> Behaviour a -> Behavior b

evalBehaviour :: Behavior Float -> Behavior (Maybe Blank.Event) -> Behaviour a -> STM a
evalBehaviour
-}
--now :: Behavior Float
--now = Now

{-

-- There are only three primitive behaviors

timerB :: Behavior Float

frameB :: Behavior Int

eventB :: Behavior Event


-- fmap :: (a -> b) -> B a -> B b

-- pure


readBehavior :: Behavior a -> STM a


-}
--newBehavior :: a -> IO (Behavior a)
--newBehavior = undefined -- atomically . fmap Behavior . newTVar

instance Show (Behavior a) where
  show _ = "Behavior{}"

loop def b = do
  a <- atomically $ evalBehavior def b
  print a
  loop (nextBehaviorEnv (case theTimer def of (a,_,_) -> a + 0.001) Nothing def) b
