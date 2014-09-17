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
  Act    :: Canvas () -> Behavior a -> (a -> Canvas Bool) -> Act


-- return True if you are finished.
--animation :: Canvas Bool -> Act
--animation = Action

-- TODO: change to act
action :: Canvas () -> Act
action m = Act m (pure ()) $ \ () -> return True

actOnBehavior :: Behavior a -> (a -> Canvas Bool) -> Act
actOnBehavior = Act (return ())

--listen :: STM () -> Act
--listen = Listen

--onEvent :: Canvas a -> Act (a -> Act a)

--  Listen       :: STM a -> Event a               -> Action

runFirstAct :: Act -> Canvas ()
{-
runFirstAct (Act m)      = m
runFirstAct (OnEvent _ k) = return ()
runFirstAct (Listen m) = return ()
runFirstAct (Acts a1 a2) = do
  runFirstAct a1
  runFirstAct a2
runFirstAct NoAct = return ()
-}
runFirstAct (Act m _ _) = m


-- run the Act; return True if you are finished
-- Still considering threading time through here.
-- it will allow a isClocked :: Act -> Bool function.
runAct :: TheBehaviorEnv -> Act -> IO (Canvas Bool)
{-
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
-}
runAct env (Act _ beh k) = do
  t <- atomically $ evalBehavior env beh
  return $ k t

--listen :: STM a -> Queue a -> Act          -- listen for mouse or keyboard
--listen stm q = Act $ (:[]) $  Listen stm q

instance Semigroup Act where
  -- may optimize for PureB.
  Act m1 b1 k1 <> Act m2 b2 k2 =
      Act (m1 >> m2)
           (liftA2 (,) b1 b2)
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

--drawTite ::

{-
class Picture picture -> Movie movie where
   directMovie :: movie picture -> (Int,Int) -> Canvas ()
-}
