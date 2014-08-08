{-# LANGUAGE OverloadedStrings, KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}
module Graphics.Storyboard.Prelude
  ( Prelude
  , runPrelude
  , wordWidth
  , imageTile
  , liftCanvas
  , pause
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text(Text)
import Graphics.Blank as Blank
import Control.Applicative
import Control.Monad
import Control.Concurrent as Concurrent

import Graphics.Storyboard.Tile
import Data.List
import Data.Text(Text)

------------------------------------------------------------------------
-- The idea behind the Prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in a simulation mode.

-- TODO: perhaps call this the Staging monad?

data Prelude :: * -> * where
  Return    :: a                              -> Prelude a
  Bind      :: Prelude a -> (a -> Prelude b)  -> Prelude b
  Canvas    :: Canvas a                       -> Prelude a
  Pause     ::                                   Prelude ()

runPrelude :: Prelude a -> Canvas a
runPrelude (Return a) = return a
runPrelude (Bind m k) = do
    a <- runPrelude m
    runPrelude (k a)
runPrelude (Canvas m) = m
runPrelude Pause = do
  liftIO $ putStrLn "pausing"
  liftIO $ Concurrent.threadDelay (1 * 1000 * 1000)
  liftIO $ putStrLn "paused"
  return ()

instance Functor Prelude where
 fmap f m = pure f <*> m

instance Applicative Prelude where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Prelude where
  return = Return
  f >>= k = Bind f k

instance MonadIO Prelude where
    liftIO = Canvas . liftIO


------------------------------------------------------------------------

pause :: Prelude ()
pause = Pause

liftCanvas :: Canvas a -> Prelude a
liftCanvas = Canvas

------------------------------------------------------------------------

-- This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: Text -> Text -> Prelude Float
wordWidth font_name txt = Canvas $ saveRestore $ do
    Blank.font $ font_name
    TextMetrics w <- measureText txt
    return w

-- This function should be memoize; it should return
-- the same answer for *every* call.
imageTile :: FilePath -> Prelude (Tile ())
imageTile filePath = Canvas $ do
    url <- liftIO $ readDataURL (mimeTypes filePath) filePath
    img <- newImage url
    return ( tile (width img, height img)
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
