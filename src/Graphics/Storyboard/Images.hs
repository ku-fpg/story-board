{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Images where

import qualified Graphics.Storyboard.Prelude as Prelude
import           Graphics.Storyboard.Slide
import           Graphics.Storyboard.Tile

-- Load an image; do not place it anywhere yet.

imageTile :: FilePath -> Slide (Tile ())
imageTile = slidePrelude . Prelude.imageTile
