{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Images where

import Graphics.Storyboard.Slide
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling
import Graphics.Storyboard.Tile
import qualified Graphics.Storyboard.Prelude as Prelude

import Control.Monad.IO.Class

import Graphics.Blank

import Control.Applicative
import Data.List
import Data.Text(Text)

-- Load an image; do not place it anywhere yet.

imageTile :: FilePath -> Slide (Tile ())
imageTile = slidePrelude . Prelude.imageTile
