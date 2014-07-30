module Graphics.Storyboard.Bling where

import Data.Text(Text)

import Graphics.Blank
import Graphics.Storyboard.Types

border :: Float -> Text -> Tile a -> Tile a
border wd col (Tile (w,h) act) = Tile (w+wd*2,h+wd*2) $ \ (w',h') -> do
  -- do the actual printing
  r <- saveRestore $ do
          translate (wd,wd)
          act (w' - wd * 2,h' - wd * 2)
  -- then print a border
  saveRestore $ do
    beginPath()
    strokeStyle col
    lineWidth 1
    rect(wd*0.5,wd*0.5,w'-wd*0.5,h'-wd*0.5)
    closePath()
    stroke()
  return r
