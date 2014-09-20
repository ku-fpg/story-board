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
import Graphics.Storyboard.Behavior

newtype Act where
 -- the bool signifies the finality of the drawing; False = more to draw
  Act :: (TheBehaviorEnv -> STM (Canvas Bool)) -> Act

-- return True if you are finished.
--animation :: Canvas Bool -> Act
--animation = Action

-- TODO: change to act
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

-----------------------------------------------------------------
{-
drawAct :: Drawing picture => Cavity Double -> picture -> Act
drawAct (Cavity loc sz) pic = action $ saveRestore $ do
    translate loc
    drawCanvas sz pic

drawMovieAct :: (Playing movie, Drawing picture) => Cavity Double -> movie picture -> Act
drawMovieAct cavity@(Cavity loc sz) movie = case wrapMovie movie of
    Movie bhr f stop -> actOnBehavior bhr cavity $ \ b -> saveRestore $ do
                                  translate loc
                                  drawCanvas sz (f b)
                                  return (stop b)
-}
