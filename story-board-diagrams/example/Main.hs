{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Diagrams.Prelude as Dia
import Diagrams.Backend.Canvas
import Graphics.Storyboard.Diagrams

import Graphics.Storyboard as SB

main :: IO ()
main = storyBoard [ slide ]

slide :: Slide ()
slide = do
      SB.align SB.center $ p $ "Diagrams Plugin Example"
      
      let t = drawTile dig (200,200)

      SB.place top t

      return ()

dig :: Diagram B R2
dig = circle 1
