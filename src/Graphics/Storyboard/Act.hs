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
  Behavior :: (TheBehaviorEnv -> STM a)
           -> Behavior a
  TimerB    :: Behavior Float
  EventB    :: Behavior (Maybe Blank.Event)

timerB    :: Behavior Float
timerB = TimerB

eventB    :: Behavior (Maybe Blank.Event)
eventB = EventB

evalBehavior ::TheBehaviorEnv -> Behavior a -> STM a
evalBehavior env (Behavior fn) = fn env
--evalBehavior env (PureB a) = return a
--evalBehavior env (AppB f a) = evalBehavior env f <*> evalBehavior env a
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
  pure = Behavior . const . return
  f <*> x = Behavior $ \ env -> evalBehavior env f <*> evalBehavior env x

sample :: STM a -> STM (Behavior a)
sample m = do
  b <- m
  var <- newTVar (b,0,b)
  return $ Behavior $ \ env -> do
      history@(new,clk,_) <- readTVar var
      if clk + 1 == theTimestamp env
      then do
        a <- m
        writeTVar var $ consHistoric a $ history
        return a
      else return $ evalHistoric env history


switch :: (a -> b -> b) -> b -> Behavior a -> STM (Behavior b)
switch f b bah = do
  var <- newTVar (b,0,b)
  return $ Behavior $ \ env -> do
        history@(new,clk,_) <- readTVar var
        if clk + 1 == theTimestamp env
        then do
          a <- evalBehavior env bah
          let newest = f a new
          writeTVar var $ consHistoric newest $ history
          return newest
        else return $ evalHistoric env history


instance Show (Behavior a) where
  show _ = "Behavior{}"

loop def b = do
  a <- atomically $ evalBehavior def b
  print a
  loop (nextBehaviorEnv (case theTimer def of (a,_,_) -> a + 0.001) Nothing def) b
