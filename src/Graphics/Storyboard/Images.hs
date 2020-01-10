{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Images where

import           Control.Monad.IO.Class
import           Data.Monoid ((<>))
import           Data.Text (pack)

import           Graphics.Blank as Blank
import           Graphics.Storyboard.Tile
import           Graphics.Storyboard.Types


-- | Load an image; do not place it anywhere yet.
imageTile :: (MonadIO m, MonadCanvas m) => FilePath -> m (Tile ())
imageTile fileName = do
  url <- liftIO $ readDataURL "image/png" fileName
  liftCanvas $ do
    img <- newImage url
    return $ tile (width img, height img) $ \ (Cavity _ _) -> drawImage (img,[0,0])

-- | Load an scaled image; do not place it anywhere yet.
scaledImageTile :: (MonadIO m, MonadCanvas m) => FilePath -> Double -> m (Tile ())
scaledImageTile fileName s = do
  url <- liftIO $ readDataURL "image/png" fileName
  liftCanvas $ do
    img <- newImage url
    return $ tile (width img * s, height img * s) $ \ (Cavity _ _) -> saveRestore $ do
          scale (s,s)
          drawImage (img,[0,0])
