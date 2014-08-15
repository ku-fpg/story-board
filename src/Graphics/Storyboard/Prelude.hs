{-# LANGUAGE StandaloneDeriving, OverloadedStrings, KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}
module Graphics.Storyboard.Prelude
  ( Prelude
  , startPrelude
  , wordWidth
  , imageTile
  , liftCanvas
  , keyPress
  , PreludeEnv(..)
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text(Text)
import Graphics.Blank as Blank
import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import System.IO
import System.Directory
import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.Storyboard.Tile
import Graphics.Storyboard.Types
import Data.List
import Data.Text(Text)

------------------------------------------------------------------------
-- The idea behind the Prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in a simulation mode.

-- TODO: perhaps call this the Staging monad?

newtype Prelude a = Prelude { runPrelude :: PreludeEnv -> Canvas a }

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

instance MonadCanvas Prelude where
  liftCanvas = Prelude . const

------------------------------------------------------------------------

data PreludeEnv = PreludeEnv
  { memoWordWidth     :: Text -> Text -> Maybe Float
  , preludeEventQueue :: EventQueue
  , recordMemo        :: Memo -> IO ()
  }

data Memo where
  MemoWordWidth :: Text -> Text -> Float -> Memo

deriving instance Show Memo
deriving instance Read Memo
deriving instance Eq Memo
deriving instance Ord Memo


startPrelude :: Prelude a -> EventQueue -> Canvas a
startPrelude p evQ = do
  memo <- liftIO $ readMemo
--  liftIO $ print memo

  let dbWordWidth = Map.fromList
          [ ((t2,t1),w)
          | MemoWordWidth t1 t2 w <- memo
          ]

  varMemo <- liftIO $ atomically $ newTVar Set.empty

  r <- runPrelude p $ PreludeEnv
    { memoWordWidth = \ t1 t2 -> Map.lookup (t2,t1) dbWordWidth
    , preludeEventQueue = evQ
    , recordMemo = \ memo -> liftIO $ atomically $ modifyTVar' varMemo $ Set.insert memo
    }

  memoSet <- liftIO $ atomically $ readTVar varMemo
  when (not (Set.null memoSet)) $ do
    liftIO $ appendFile memoFile $ unlines $ map show $ Set.toList $ memoSet

  -- Write the extras to the cache
  return r


memoFile :: FilePath
memoFile = ".story-board-memo"

readMemo :: IO [Memo]
readMemo = do
  b <- doesFileExist memoFile
  if not b
  then return []
  else do
    txt <- liftIO $ readFile memoFile
    return [ read ln
           | ln <- lines txt
           ]

------------------------------------------------------------------------


-- | pause for key press
keyPress :: Prelude ()
keyPress = Prelude $ \ ch -> liftIO $ atomically $ do
  event <- readTChan (preludeEventQueue ch)
  if eType event == "keypress"
  then return ()
  else retry

------------------------------------------------------------------------

-- This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: Text -> Text -> Prelude Float
wordWidth font_name txt = Prelude $ \ env -> do
  case memoWordWidth env font_name txt of
    Nothing -> do
      TextMetrics w <- saveRestore $ do
        Blank.font $ font_name
        measureText txt
      liftIO $ recordMemo env $ MemoWordWidth font_name txt w
      return w
    Just w -> return w

-- This function should be memoize; it should return
-- the same answer for *every* call.
imageTile :: FilePath -> Prelude (Tile ())
imageTile filePath = liftCanvas $ do
    url <- liftIO $ readDataURL (mimeTypes filePath) filePath
    img <- newImage url
    return ( tile (width img, height img)
           $ const
           $ const
           $ liftCanvas
           $ drawImage (img,[0,0])
           )

------------------------------------------------------------------------

mimeTypes :: FilePath -> Text
mimeTypes filePath
  | ".jpg" `isSuffixOf` filePath = "image/jpeg"
  | ".png" `isSuffixOf` filePath = "image/png"
  | ".gif" `isSuffixOf` filePath = "image/gif"
  | otherwise = error $ "do not understand mime type for : " ++ show filePath
