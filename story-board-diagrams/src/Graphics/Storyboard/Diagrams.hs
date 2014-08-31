{-# LANGUAGE NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.Storyboard.Diagrams where

-- This is the diagrams plugin

import Graphics.Storyboard.Types

import Diagrams.Backend.Canvas
import Diagrams.Prelude

instance Drawing (Diagram Canvas R2) where
   drawCanvas (w,h) d = renderDia Canvas opts d
    where opts = CanvasOptions $ Dims (fromIntegral $ round w)
    	       	 	       	      (fromIntegral $ round h)
