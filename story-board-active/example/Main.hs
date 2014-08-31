{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Storyboard.Active as A

import Graphics.Blank             as B
import Graphics.Storyboard        as SB

main :: IO ()
main = SB.storyBoard [ slide ]

slide :: SB.Slide ()
slide = do
      SB.align SB.center $ SB.p $ "Active Plugin Example"
{-
      let t = SB.drawTile (200,200) dig

      SB.place SB.top (SB.nudge SB.top SB.center t)

      let t = SB.drawMovieTile (200,200) (pad 1.05 . tournament . succ. (`mod` 10) . (floor :: (Float -> Int)))

      SB.place SB.top (SB.nudge SB.top SB.center t)
-}
      return ()
