{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Storyboard.Diagrams where

-- This is the diagrams plugin

import Graphics.Storyboard.Types

import Diagrams.Backend.Canvas
import Diagrams.Prelude

instance Drawing (Diagram Canvas R2) where
   drawCanvas (w,h) d = renderDia Canvas opts d
    where opts :: Options Canvas R2
          opts = CanvasOptions $ Dims (fromIntegral w')
                                      (fromIntegral h')
          w', h' :: Int
          w' = round w
          h' = round h