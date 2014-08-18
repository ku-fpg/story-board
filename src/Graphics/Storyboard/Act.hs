{-# LANGUAGE KindSignatures, TupleSections, GADTs,
     GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Act where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Graphics.Blank

import Graphics.Storyboard.Types

newtype Act = Act { runAct :: [Action] }

data Action where
  Action       :: Canvas ()                      -> Action
  Replay       :: Float -> (Float -> Canvas ())  -> Action
  Listen       :: STM a -> Queue a               -> Action

action :: Canvas () -> Act
action = Act . (:[]) . Action

replay       :: Float -> (Float -> Canvas ()) -> Act
replay dur k = Act $ (:[]) $  Replay dur k

listen :: STM a -> Queue a -> Act          -- listen for mouse or keyboard
listen stm q = Act $ (:[]) $  Listen stm q

instance Semigroup Act where
  Act xs <> Act ys = Act (xs ++ ys)

instance Monoid Act where
  mempty = Act []
  Act xs `mappend` Act ys = Act (xs ++ ys)

-----------------------------------------------------------------

-- The actor's queue.

data Queue a = Queue { theQueue :: TVar (Maybe a) }

newQueue :: IO (Queue a)
newQueue = atomically $ fmap Queue $ newTVar $ Nothing


-- replay :: (Int,Int) -> (Int -> Canvas ()) -> Act
-- act    :: Canvas () -> Act
-- react  :: Queue a -> (a -> Canvas ()) -> Act
-- listen :: IO a -> Queue a -> Act          -- listen for mouse or keyboard
