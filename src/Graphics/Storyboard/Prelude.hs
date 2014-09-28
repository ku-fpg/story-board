{-# LANGUAGE StandaloneDeriving, OverloadedStrings, KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}
module Graphics.Storyboard.Prelude
  ( Prelude
  , startPrelude
  , wordWidth
  , liftCanvas
  , keyPress
  , bgLinear
  , mimeTypes
  , PreludeEnv(..)
  ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.List
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Text (Text, pack)

import           Graphics.Blank as Blank
import           Graphics.Storyboard.Tile
import           Graphics.Storyboard.Types

import           System.Directory

------------------------------------------------------------------------
-- The idea behind the Prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in a simulation mode.

-- TODO: perhaps call this the Staging monad?

newtype Prelude a = Prelude { runPrelude :: PreludeEnv -> IO a }

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
    liftIO = Prelude . const

------------------------------------------------------------------------

instance MonadCanvas Prelude where
  liftCanvas m = Prelude $ \ env -> send (preludeContext env) m

------------------------------------------------------------------------

data PreludeEnv = PreludeEnv
  { memoWordWidth     :: Text -> Text -> Maybe Double
  , preludeContext    :: DeviceContext
  , recordMemo        :: Memo -> IO ()
  }

data Memo where
  MemoWordWidth :: Text -> Text -> Double -> Memo

deriving instance Show Memo
deriving instance Read Memo
deriving instance Eq Memo
deriving instance Ord Memo


startPrelude :: Prelude a -> DeviceContext -> IO a
startPrelude p context = do
  memo <- liftIO $ readMemo
--  liftIO $ print memo

  let dbWordWidth = Map.fromList
          [ ((t2,t1),w)
          | MemoWordWidth t1 t2 w <- memo
          ]

  varMemo <- liftIO $ atomically $ newTVar Set.empty

  r <- runPrelude p $ PreludeEnv
    { memoWordWidth = \ t1 t2 -> Map.lookup (t2,t1) dbWordWidth
    , preludeContext = context
    , recordMemo = \ memo' -> liftIO $ atomically $ modifyTVar' varMemo $ Set.insert memo'
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
keyPress = do
 liftIO $ putStrLn "waiting"
 Prelude $ \ ch -> liftIO $ atomically $ do
  event <- readTChan (eventQueue $ preludeContext ch)
  if eType event == "keypress"
  then return ()
  else retry
 liftIO $ putStrLn "waited"

------------------------------------------------------------------------

-- | This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: Text -> Text -> Prelude Double
wordWidth font_name txt = Prelude $ \ env -> do
  -- It should be possible to write this without breaking the monad abstraction
  case memoWordWidth env font_name txt of
    Nothing -> do
      TextMetrics w <- send (preludeContext env) $ saveRestore $ do
        Blank.font $ font_name
        measureText txt
      liftIO $ recordMemo env $ MemoWordWidth font_name txt w
      return w
    Just w -> return w

------------------------------------------------------------------------

bgLinear :: Text -> Text -> Prelude Background
bgLinear c0 c1 = liftCanvas $ do
  -- We normalize the gradient to 1x1, because we can then cache them
  -- TODO: generalize to different directions
  grd <- createLinearGradient(0, 0, 0, 1)
  grd # addColorStop(0, c0)
  grd # addColorStop(1, c1)
  return $ Background grd

------------------------------------------------------------------------

mimeTypes :: FilePath -> Text
mimeTypes filePath
  | ".jpg" `isSuffixOf` filePath = "image/jpeg"
  | ".png" `isSuffixOf` filePath = "image/png"
  | ".gif" `isSuffixOf` filePath = "image/gif"
  | otherwise = error $ "do not understand mime type for : " ++ show filePath
