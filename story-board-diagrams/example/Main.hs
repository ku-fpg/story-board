{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Diagrams.Prelude as Dia
import Diagrams.Backend.Canvas
import Graphics.Storyboard.Diagrams

import qualified Graphics.Storyboard as SB

main :: IO ()
main = SB.storyBoard [ slide ]

slide :: SB.Slide ()
slide = do
      SB.align SB.center $ SB.p $ "Diagrams Plugin Example"

      let t = SB.drawTile (200,200) dig

      SB.place SB.top (SB.nudge SB.top SB.center t)

      let t = SB.drawMovieTile (200,200) (pad 1.05 . tournament . succ. (`mod` 10) . (floor :: (Float -> Int)))

      SB.place SB.top (SB.nudge SB.top SB.center t)

      return ()

dig :: Diagram B R2
dig = tournament 6

node :: Int -> Diagram B R2
node n = text (show n) # fontSizeN 0.1 # fc white
      <> circle 0.2 # fc green # named n

arrowOpts = with & gaps  .~ small
                 & headLength .~ Global 0.2

tournament :: Int -> Diagram B R2
tournament n = decorateTrail (regPoly n 1) (map node [1..n])
  # applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]
