{-# LANGUAGE OverloadedStrings #-}
module Graphics.Storyboard.Bling where

import Data.Text(Text)

import Graphics.Blank
import qualified Graphics.Blank.Style as Style

import Graphics.Storyboard.Slide
import Graphics.Storyboard.Tile

border :: Float -> Text -> Tile a -> Tile a
border wd col (Tile (w,h) act) = Tile (w+wd*2,h+wd*2) $ \ (w',h') -> do

  saveRestore $ do
    grd <- createLinearGradient(0, 0, 0, h')
    -- light blue
    grd # addColorStop(0, "#8EA154")
    -- dark blue
    grd # addColorStop(1, "#7D8E4B")
    Style.fillStyle grd;

    beginPath()

    shadowColor "black"
    shadowOffsetX 5
    shadowOffsetY 5
    shadowBlur 10

    rect(0,0,w',h')
    closePath()
    lineWidth wd
    fill()

  -- do the actual printing
  r <- saveRestore $ do
          translate (wd,wd)
          act (w' - wd * 2,h' - wd * 2)
  -- then print a border
  saveRestore $ do
    beginPath()
    rect(0,0,w',h')
    closePath()
    lineWidth wd
    strokeStyle col
    stroke()

  return r




--boxes :: Size Float -> Story () -> Tile ()
