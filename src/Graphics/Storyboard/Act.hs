{-# LANGUAGE KindSignatures, TupleSections, GADTs,
     GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Act where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Graphics.Blank hiding (Event)

import Graphics.Storyboard.Types

--newtype Act = Act { runAct :: [Action] }

data Act where
    -- | the bool signifies the finality of the drawing; False = more to draw
  Act     :: Canvas ()                     -> Act
  OnEvent :: Behavior a -> (a -> Canvas Bool) -> Act
  Listen  :: STM a -> Behavior a         -> Act

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


listen :: STM a -> Behavior a -> Act
listen = Listen

--onEvent :: Canvas a -> Act (a -> Act a)

--  Listen       :: STM a -> Event a               -> Action

runFirstAct :: Act -> Canvas ()
runFirstAct (Act m)      = m
runFirstAct (OnEvent _ k) = return ()
runFirstAct (Acts a1 a2) = do
  runFirstAct a1
  runFirstAct a2
runFirstAct NoAct = return ()


-- run the Act; return True if you are finished
-- Still considering threading time through here.
-- it will allow a isClocked :: Act -> Bool function.
runAct :: Act -> Canvas Bool
runAct (Act m)         = return True
runAct (OnEvent (Behavior b) k) = do
      t <- liftIO $ atomically $ readTVar b
      k t
runAct (Acts a1 a2) = do
  r1 <- runAct a1
  r2 <- runAct a2
  return $ r1 && r2
runAct NoAct = return True


-- This STM only succeeds if one of STM succeeded
runListen :: Act -> STM ()
runListen (Act m) = return ()
runListen (OnEvent _ k) = return ()
runListen (Listen m v) = m >>= setBehavior v
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

-- The actor's queue.

data Behavior :: * -> * where
  Behavior :: TVar a -> Behavior a
--  Now   ::              Event Float

--now :: Behavior Float
--now = Now

newBehavior :: a -> IO (Behavior a)
newBehavior = atomically . fmap Behavior . newTVar

instance Show (Behavior a) where
  show _ = "Behavior{}a"


setBehavior :: Behavior a -> a -> STM ()
setBehavior (Behavior v) = writeTVar v

-- replay :: (Int,Int) -> (Int -> Canvas ()) -> Act
-- act    :: Canvas () -> Act
-- react  :: Queue a -> (a -> Canvas ()) -> Act
-- listen :: IO a -> Queue a -> Act          -- listen for mouse or keyboard
