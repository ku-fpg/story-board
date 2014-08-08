{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}
module Graphics.Storyboard.Prelude where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text(Text)
import Graphics.Blank as Blank

import Graphics.Storyboard.Tile
import Data.List
import Data.Text(Text)

-- The idea behind the Prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in a simulation mode.

newtype Prelude a = Prelude { runPrelude :: Canvas a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: Text -> Text -> Prelude Float
wordWidth font_name txt = Prelude $ saveRestore $ do
    Blank.font $ font_name
    TextMetrics w <- measureText txt
    return w

-- This function should be memoize; it should return
-- the same answer for *every* call.
imageTile :: FilePath -> Prelude (Tile ())
imageTile filePath = Prelude $ do
    url <- liftIO $ readDataURL (mimeTypes filePath) filePath
    img <- newImage url
    return ( tile (width img, height img)
           $ const
           $ drawImage (img,[0,0])
           )

mimeTypes :: FilePath -> Text
mimeTypes filePath
  | ".jpg" `isSuffixOf` filePath = "image/jpeg"
  | ".png" `isSuffixOf` filePath = "image/png"
  | ".gif" `isSuffixOf` filePath = "image/gif"
  | otherwise = error $ "do not understand mime type for : " ++ show filePath
