{-# LANGUAGE KindSignatures, TupleSections, GADTs,
     GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Act where

import Control.Concurrent.STM
import Control.Monad

import Data.Semigroup

import Graphics.Blank hiding (Event)
import Graphics.Storyboard.Behavior

newtype Act where
 -- the bool signifies the finality of the drawing; False = more to draw
 -- The Canvas actions are scoped with a saveRestore, and are translated
 -- (in Tile) so that you can assume the top corner is (0,0)
  Act :: (TheBehaviorEnv -> STM (Canvas Bool)) -> Act

action :: Canvas () -> Act
action m = Act $ \ _ -> return (fmap (const True) m)

actOnBehavior :: (TheBehaviorEnv -> STM (Canvas Bool)) -> Act
actOnBehavior = Act

runAct :: TheBehaviorEnv -> Act -> STM (Canvas Bool)
runAct env (Act k) = k env

instance Semigroup Act where
  -- may optimize for PureB.
  Act k1 <> Act k2 = Act $ \ env -> liftM2 (liftM2 (&&)) (k1 env) (k2 env)

instance Monoid Act where
  mempty = action (return ())
  mappend = (<>)
