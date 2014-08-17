{-# LANGUAGE KindSignatures, TupleSections, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Act where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Semigroup
import Graphics.Blank


import Graphics.Storyboard.Types

newtype Act = Act { runAct :: [Action] }

data Action where
  Action       :: Canvas ()        -> Action
--  NextAnimationFrame :: Act -> NextAct

action :: Canvas () -> Act
action = Act . (:[]) . Action

instance Semigroup Act where
  Act xs <> Act ys = Act (xs ++ ys)

instance Monoid Act where
  mempty = Act []
  Act xs `mappend` Act ys = Act (xs ++ ys)

--nextAnimationFrame :: Act () -> Act ()
--nextAnimationFrame = Act .return . ((),) . (:[]) . NextAnimationFrame
{-
instance MonadCanvas Act where
  liftCanvas m = Act $ fmap (,[]) m

instance Functor Act where
 fmap f m = pure f <*> m

instance Applicative Act where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Act where
  return = Act . return . (,[])
  Act m >>= k = Act $ do
      (a,nx1) <- m
      (r,nx2) <- runAct (k a)
      return (r,nx1 ++ nx2)

instance MonadIO Act where
    liftIO = Act . fmap (,[]) . liftIO
-}


-- replay :: (Int,Int) -> (Int -> Canvas ()) -> Act
-- act    :: Canvas () -> Act
-- react  :: Queue a -> (a -> Canvas ()) -> Act
-- listen :: IO a -> Queue a -> Act          -- listen for mouse or keyboard
