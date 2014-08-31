{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
module Graphics.Storyboard.Active where

-- This is the active plugin

import Graphics.Storyboard.Behavior

import Data.Active

-- Right now, this is only correct for Active's starting at time 0.
instance Playing Active where
  wrapMovie :: Active picture -> Movie picture
  wrapMovie act =
      Movie
        timerB
        (runActive act . toTime)
        (case activeEra act of
           Nothing -> const False
           Just era -> \ b -> end era <= toTime b
        )
