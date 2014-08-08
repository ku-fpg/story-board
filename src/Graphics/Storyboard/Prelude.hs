{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}
module Graphics.Storyboard.Prelude
  ( Prelude
  , runPrelude
  , wordWidth
  , imageTile
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text(Text)
import Graphics.Blank as Blank
import Control.Applicative
import Control.Monad

import Graphics.Storyboard.Tile
import Data.List
import Data.Text(Text)

-- The idea behind the Prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in a simulation mode.

-- TODO: perhaps call this Staging?

data Prelude :: * -> * where
  Prelude :: Canvas a -> Prelude a

runPrelude :: Prelude a -> Canvas a
runPrelude (Prelude m) = m

instance Functor Prelude where
 fmap f m = pure f <*> m

instance Applicative Prelude where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Prelude where
  return = Prelude . return
  Prelude f >>= k = Prelude $ do
    a <- f
    runPrelude (k a)

instance MonadIO Prelude where
    liftIO = Prelude . liftIO

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
