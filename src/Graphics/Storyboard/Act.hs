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

data Act where
 -- the bool signifies the finality of the drawing; False = more to draw
  Act    :: Behavior a -> (a -> Canvas Bool) -> Act


-- return True if you are finished.
--animation :: Canvas Bool -> Act
--animation = Action

-- TODO: change to act
action :: Canvas () -> Act
action m = Act (pure ()) $ \ () -> m >> return True

actOnBehavior :: Behavior a -> (a -> Canvas Bool) -> Act
actOnBehavior = Act


-- run the Act; return True if you are finished
-- Still considering threading time through here.
-- it will allow a isClocked :: Act -> Bool function.
runAct :: TheBehaviorEnv -> Act -> IO (Canvas Bool)
runAct env (Act beh k) = do
  t <- atomically $ evalBehavior env beh
  return $ k t

instance Semigroup Act where
  -- may optimize for PureB.
  Act b1 k1 <> Act b2 k2 =
      Act (liftA2 (,) b1 b2)
          (\ (a,b) -> do liftM2 (&&) (k1 a) (k2 b))


instance Monoid Act where
  mempty = action (return ())
  mappend = (<>)

-----------------------------------------------------------------

drawAct :: Drawing picture => Size Float -> picture -> Act
drawAct sz pic = action $ drawCanvas sz pic

drawMovieAct :: (Playing movie, Drawing picture) => Size Float -> movie picture -> Act
drawMovieAct sz movie = case wrapMovie movie of
    Movie bhr f stop -> actOnBehavior bhr (\ b -> drawCanvas sz (f b) >> return (stop b))
