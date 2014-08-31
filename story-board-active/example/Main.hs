{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Storyboard.Active() -- for the instance

import Graphics.Blank             as B
import Graphics.Storyboard        as SB

import Data.Active

main :: IO ()
main = SB.storyBoard [ slide ]

slide :: SB.Slide ()
slide = do
      SB.align SB.center $ SB.p $ "Active Plugin Example"

      let t = SB.drawMovieTile (200,200) $ mkActive 0 10 $ \ tm -> do
                  beginPath()
                  rect(1,1,20 * fromTime tm,20 * fromTime tm)
                  strokeStyle "red"
                  lineWidth 1
                  closePath()
                  stroke()

      SB.place SB.top (SB.nudge SB.center SB.center t)

      return ()
