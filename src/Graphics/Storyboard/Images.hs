{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Images where

import Graphics.Storyboard.Types
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling
import Control.Monad.IO.Class

import Graphics.Blank

import Data.List
import Data.Text(Text)


imageTile :: FilePath -> Canvas (Tile ())
imageTile filePath = do
    url <- liftIO $ readDataURL (mimeTypes filePath) filePath
    img <- newImage url
    return $ tile (width img, height img)
          $ const
          $ drawImage (img,[0,0])

mimeTypes :: FilePath -> Text
mimeTypes filePath
  | ".jpg" `isSuffixOf` filePath = "image/jpeg"
  | ".png" `isSuffixOf` filePath = "image/png"
  | otherwise = error $ "do not understand mime type for : " ++ show filePath
