{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Environment where

import Data.Text(Text)

import Graphics.Storyboard.Literals

data Environment = Environment
  {  baseFont    :: Text      -- which font, "sans-serif"
  ,  fontSize    :: Int       -- how big, 10
  ,  spaceWidth  :: Float     -- size of space, 3.0 (perhaps 2.8)
  ,  baseColor   :: Text      -- current color
  ,  baseAlign   :: Alignment -- What alignment method are we using
  ,  baseOffset  :: Float     -- size of offset from baseline
  ,  globalDPR   :: Float     -- scaling of whole page; Device Pixel Ratio.
--  ,  lineWidth   :: Float   -- default = 1
  ,  _foo        :: Int
  , leftMargin   :: Float     -- default 0
  , rightMargin  :: Float     -- default 0
  , tabSize      :: Float     -- how much to inc the margin by
  , afterParagraph :: Float
  }
  deriving (Show)

-- $(makeLenses ''MarkupContext)

defaultContext :: Environment
defaultContext = Environment "sans-serif" 32 (2.6 * 3.2) "black" left 0 1 0 0 0 50 10

{-
class Context a where
  align :: Alignment -> a -> a    -- might be seperate, because of prose
--  font_ :: Text      -> a -> a
  raisebox :: Float -> a -> a
--  sub   :: a -> a
  color :: Text      -> a -> a
  size  :: Int       -> a -> a
--  set size $
  context :: (Environment -> a) -> a -> a

instance Context Environment where
  align j m = m { baseAlign = j }
  color c m = m { baseColor = c }
  raisebox o m = m { baseOffset = baseOffset m + o }
  size  i m = m { fontSize = i, spaceWidth = fromIntegral i * 0.28 }
  context f m = f m
-}

instance Markup Environment where


--class Align a where
--  align :: Alignment -> a -> a

class Layout a where
  scoped :: (Environment -> Environment) -> a -> a

itemize :: Layout a => a -> a
itemize = scoped $ \ cxt -> cxt { leftMargin = leftMargin cxt + tabSize cxt }

align :: Layout a => Alignment -> a -> a
align j = scoped $ \ m -> m { baseAlign = j }
