module Graphics.Storyboard.Bling where

import Data.Text(Text)

import Graphics.Blank
import Graphics.Storyboard.Slide
import Graphics.Storyboard.Tile


border :: Float -> Text -> Tile a -> Tile a
border wd col (Tile (w,h) act) = Tile (w+wd*2,h+wd*2) $ \ (w',h') -> do
  -- do the actual printing
  r <- saveRestore $ do
          translate (wd,wd)
          act (w' - wd * 2,h' - wd * 2)
  -- then print a border
  saveRestore $ do
    beginPath()
{-
    shadowColor "pink"
    shadowOffsetX 5
    shadowOffsetY 5
    shadowBlur 0
-}
    strokeStyle col
    lineWidth 1
    rect(0,0,w',h')
    closePath()
    stroke()
  return r
