{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Images where

import Graphics.Storyboard.Types
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling
import Control.Monad.IO.Class

import Graphics.Blank

import Control.Applicative
import Data.List
import Data.Text(Text)

-- Load an image; do not place it anywhere yet.

imageTile :: FilePath -> Story (Tile ())
imageTile filePath = Story $ \ cxt sz -> fmap (\ a -> (a,pure ())) $ Prelude $ do
    url <- liftIO $ readDataURL (mimeTypes filePath) filePath
    img <- newImage url
    return $ tile (width img, height img)
          $ const
          $ drawImage (img,[0,0])

mimeTypes :: FilePath -> Text
mimeTypes filePath
  | ".jpg" `isSuffixOf` filePath = "image/jpeg"
  | ".png" `isSuffixOf` filePath = "image/png"
  | ".gif" `isSuffixOf` filePath = "image/gif"
  | otherwise = error $ "do not understand mime type for : " ++ show filePath
