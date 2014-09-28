{-# LANGUAGE KindSignatures, TupleSections, GADTs,
     GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Act where

import Control.Concurrent.STM
import Control.Monad

import Data.Semigroup

import Graphics.Blank hiding (Event)
import Graphics.Storyboard.Behavior

data Act where
 -- the bool signifies the finality of the drawing; False = more to draw
 -- The Canvas actions are scoped with a saveRestore, and are translated
 -- (in Tile) so that you can assume the top corner is (0,0)
  Act :: (TheBehaviorEnv -> STM (Canvas Bool)) -> Act
  Scenery :: Canvas () -> Act

action :: Canvas () -> Act
action = Scenery

actOnBehavior :: (TheBehaviorEnv -> STM (Canvas Bool)) -> Act
actOnBehavior = Act

runAct :: TheBehaviorEnv -> Act -> STM (Canvas Bool)
runAct env (Act k) = k env
runAct env (Scenery m) = return (m >> return True)

getScenery :: Act -> Maybe (Canvas ())
getScenery (Scenery m) = Just m
getScenery _           = Nothing

instance Semigroup Act where
  -- may optimize for PureB.
  Scenery m1 <> Scenery m2 = Scenery (m1 >> m2)
  Scenery m1 <> Act k2 = Act $ \ env -> do { r2 <- k2 env ; return (m1 >> r2) }
  Act k1 <> Scenery m2 = Act $ \ env -> do { r1 <- k1 env ; return (do { r <- r1 ; m2 ; return r })}
  Act k1 <> Act k2 = Act $ \ env -> liftM2 (liftM2 (&&)) (k1 env) (k2 env)

instance Monoid Act where
  mempty = action (return ())
  mappend = (<>)
