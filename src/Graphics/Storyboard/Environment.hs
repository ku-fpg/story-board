{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Environment where

import Data.Text(Text)

import Graphics.Storyboard.Literals

-- | The Environment is a grab-bag of (scoped) constants.

data Environment = Environment
  {  theFont            :: Text      -- ^ which font, "sans-serif"
  ,  theFontSize        :: Int       -- ^ how big, 32
  ,  theSpaceWidth      :: Float     -- ^ size of space, 0.26 * 32
  ,  theColor           :: Text      -- ^ current color, black
  ,  theAlignment       :: Alignment -- ^ What alignment method are we using, left
--  ,  theBaselineOffset  :: Float     -- ^ size of offset from baseline, 0
--  ,  theDevicePixelRatio :: Float     -- scaling of whole page; Device Pixel Ratio.
  ,  theLineWidth   :: Float     -- ^ line width, 1
  , theLeftMargin   :: Float     -- ^ left margin, 0
  , theRightMargin  :: Float     -- ^ right margin, 0
  , theTabSize      :: Float     -- ^ indent size, 50
  , theParSkip      :: Float     -- ^ Space before each paragraph, 10
  , theLigatures    :: [(Text,Text)]
  }
  deriving Show

defaultEnvironment :: Environment
defaultEnvironment = Environment
  {  theFont            = "sans-serif"
  ,  theFontSize        = 32
  ,  theSpaceWidth      = 0.26 * 32
  ,  theColor           = "black"
  ,  theAlignment       = left
--  ,  theBaselineOffset  = 0
--  ,  theDevicePixelRatio :: Float     -- scaling of whole page; Device Pixel Ratio.
  ,  theLineWidth       = 1
  , theLeftMargin       = 0
  , theRightMargin      = 0
  , theTabSize          = 50
  , theParSkip          = 10
  , theLigatures        = [] -- [("ff","\xfb00"),("fi","\xfb01")]
  }


instance Markup Environment where
  font     f m = m { theFont = f }
  fontSize n m = m { theFontSize = n, theSpaceWidth = 0.26 * fromIntegral n }

class Layout a where
  scoped :: (Environment -> Environment) -> a -> a

indent :: Layout a => a -> a
indent = scoped $ \ cxt -> cxt { theLeftMargin = theLeftMargin cxt + theTabSize cxt }

align :: Layout a => Alignment -> a -> a
align j = scoped $ \ m -> m { theAlignment = j }

parskip :: Layout a => Float -> a -> a
parskip n = scoped $ \ m -> m { theParSkip = n }
