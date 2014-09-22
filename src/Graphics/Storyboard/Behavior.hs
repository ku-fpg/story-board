{-# LANGUAGE KindSignatures, TupleSections, GADTs,
     GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings,
     ExistentialQuantification, FlexibleInstances #-}

module Graphics.Storyboard.Behavior where

import           Control.Applicative
import           Control.Concurrent.STM

import qualified Graphics.Blank as Blank
import           Graphics.Blank hiding (Event)
import           Graphics.Storyboard.Types

-----------------------------------------------------------------


-- The assumption is that the history timestamps are the same
-- as the main timestamp.

data TheBehaviorEnv = TheBehaviorEnv
  { theTimer  :: Historic Double
  , theEvent  :: Historic (Maybe Blank.Event)
  , theTimestamp :: Timestamp
--  , theBehaviorCavity :: Cavity Double
  }

defaultBehaviorEnv :: TheBehaviorEnv
defaultBehaviorEnv = TheBehaviorEnv
  { theTimer  = (0,0,0)
  , theEvent  = (Nothing,0,Nothing)
  , theTimestamp = 0
--  , theBehaviorCavity
  }

nextBehaviorEnv :: Double -> Maybe Blank.Event -> TheBehaviorEnv -> TheBehaviorEnv
nextBehaviorEnv t e env = TheBehaviorEnv
  { theTimer  = consHistoric t $ theTimer env
  , theEvent  = consHistoric e $ theEvent env
  , theTimestamp = theTimestamp env + 1
  }

type Timestamp = Int
type Historic a = (a,Timestamp,a)

data Behavior :: * -> * where
  Behavior :: (Cavity Double -> TheBehaviorEnv -> STM a)
           -> Behavior a
  TimerB    :: Behavior Double
  EventB    :: Behavior (Maybe Blank.Event)
  CavityB   :: Behavior (Cavity Double)
  PureB     :: a -> Behavior a

timerB    :: Behavior Double
timerB = TimerB

eventB    :: Behavior (Maybe Blank.Event)
eventB = EventB

cavityB = CavityB
cavityB   :: Behavior (Cavity Double)

evalBehavior :: Cavity Double -> TheBehaviorEnv -> Behavior a -> STM a
evalBehavior cavity env (Behavior fn) = fn cavity env
evalBehavior _      env TimerB        = return $ evalHistoric env (theTimer env)
evalBehavior _      env EventB        = return $ evalHistoric env (theEvent env)
evalBehavior cavity _   CavityB       = return $ cavity
evalBehavior _      _   (PureB a)     = return a

translateBehavior :: Coord Double -> Behavior (Canvas a) -> Behavior (Canvas a)
translateBehavior (x,y) b = (\ m -> saveRestore $ do { translate (x,y) ; m }
                            ) <$> b


evalHistoric :: TheBehaviorEnv -> Historic a -> a
evalHistoric env (new,clk,old)
    | clk - 1 == theTimestamp env = old
    | clk     == theTimestamp env = new
    | otherwise                   = error "not enough history for behaviour"

consHistoric :: a -> Historic a -> Historic a
consHistoric a2 (a1,t,_) = (a2,t+1,a1)

instance Functor Behavior where
  fmap f b = pure f <*> b

instance Applicative Behavior where
  pure = PureB
  PureB f <*> PureB x = PureB $ f x
  f <*> x = Behavior $ \ cav env -> evalBehavior cav env f <*> evalBehavior cav env x

sample :: STM a -> STM (Behavior a)
sample m = do
  b <- m
  var <- newTVar (b,0,b)
  return $ Behavior $ \ _ env -> do
      history@(_,clk,_) <- readTVar var
      if clk + 1 == theTimestamp env
      then do
        a <- m
        writeTVar var $ consHistoric a $ history
        return a
      else return $ evalHistoric env history


switch :: (a -> b -> b) -> b -> Behavior a -> STM (Behavior b)
switch f b bah = do
  var <- newTVar (b,0,b)
  return $ Behavior $ \ cav env -> do
        history@(new,clk,_) <- readTVar var
        if clk + 1 == theTimestamp env
        then do
          a <- evalBehavior cav env bah
          let newest = f a new
          writeTVar var $ consHistoric newest $ history
          return newest
        else return $ evalHistoric env history


instance Show (Behavior a) where
  show _ = "Behavior{}"


data Movie p = forall b . Movie
  { movieBehavior :: Behavior b
  , movieSnapshot :: b -> p
  , movieStop     :: b -> Bool
  }

class Playing movie where
  wrapMovie :: movie picture -> Movie picture

instance Playing ((->) Double) where
   wrapMovie f = Movie timerB f (const False)
