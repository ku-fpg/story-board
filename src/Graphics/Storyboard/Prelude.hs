{-# LANGUAGE OverloadedStrings, KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}
module Graphics.Storyboard.Prelude
  ( Prelude
  , runPrelude
  , wordWidth
  , imageTile
  , liftCanvas
  , keyPress
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text(Text)
import Graphics.Blank as Blank
import Control.Applicative
import Control.Monad
import Control.Concurrent.STM

import Graphics.Storyboard.Tile
import Data.List
import Data.Text(Text)

------------------------------------------------------------------------
-- The idea behind the Prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in a simulation mode.

-- TODO: perhaps call this the Staging monad?

newtype Prelude a = Prelude { runPrelude :: EventQueue -> Canvas a }

instance Functor Prelude where
 fmap f m = pure f <*> m

instance Applicative Prelude where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Prelude where
  return a = Prelude (const $ return a)
  Prelude m >>= k = Prelude $ \ q -> do
    r <- m q
    runPrelude (k r) q

instance MonadIO Prelude where
    liftIO = Prelude . const . liftIO

------------------------------------------------------------------------

liftCanvas :: Canvas a -> Prelude a
liftCanvas = Prelude . const

------------------------------------------------------------------------

-- | pause for key press
keyPress :: Prelude ()
keyPress = Prelude $ \ ch -> liftIO $ atomically $ do
  event <- readTChan ch
  if eType event == "keypress"
  then return ()
  else retry

------------------------------------------------------------------------

-- This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: Text -> Text -> Prelude Float
wordWidth font_name txt = liftCanvas $ saveRestore $ do
    Blank.font $ font_name
    TextMetrics w <- measureText txt
    return w

-- This function should be memoize; it should return
-- the same answer for *every* call.
imageTile :: FilePath -> Prelude (Tile ())
imageTile filePath = liftCanvas $ do
    url <- liftIO $ readDataURL (mimeTypes filePath) filePath
    img <- newImage url
    return ( tile (width img, height img)
           $ const
           $ const
           $ drawImage (img,[0,0])
           )

------------------------------------------------------------------------

mimeTypes :: FilePath -> Text
mimeTypes filePath
  | ".jpg" `isSuffixOf` filePath = "image/jpeg"
  | ".png" `isSuffixOf` filePath = "image/png"
  | ".gif" `isSuffixOf` filePath = "image/gif"
  | otherwise = error $ "do not understand mime type for : " ++ show filePath
