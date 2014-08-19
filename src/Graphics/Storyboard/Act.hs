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
  Action       :: Canvas Bool              -> Act
--  Replay       :: Float -> (Float -> Canvas ())  -> Act
  OnEvent      :: Event a -> (a -> Act)    -> Act
  Acts         :: Act -> Act               -> Act
  NoAct        ::                             Act

-- return True if you are finished.
animation :: Canvas Bool -> Act
animation = Action

action :: Canvas () -> Act
action = animation . fmap (const True)

--onEvent :: Canvas a -> Act (a -> Act a)

--  Listen       :: STM a -> Event a               -> Action

-- run the Act; return True if you are finished
runAct :: Float -> Act -> Canvas Bool
runAct t (Action m)     = m
runAct t (OnEvent Now k) = runAct t (k t)
runAct t (Acts a1 a2) = do
  r1 <- runAct t a1
  r2 <- runAct t a2
  return $ r1 && r2
runAct t NoAct = return True

--actAct ::

--replay       :: Float -> (Float -> Canvas ()) -> Act
--replay dur k = Replay dur k

onEvent :: Event a -> (a -> Act) -> Act
onEvent = OnEvent

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

data Event :: * -> * where
  Event :: TVar (Maybe a) -> Event a
  Now   ::                   Event Float

now :: Event Float
now = Now

newEvent :: IO (Event a)
newEvent = atomically $ fmap Event $ newTVar $ Nothing


-- replay :: (Int,Int) -> (Int -> Canvas ()) -> Act
-- act    :: Canvas () -> Act
-- react  :: Queue a -> (a -> Canvas ()) -> Act
-- listen :: IO a -> Queue a -> Act          -- listen for mouse or keyboard
