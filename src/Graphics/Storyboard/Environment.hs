{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Environment where

import Data.Text(Text)

import Graphics.Storyboard.Literals
import Graphics.Storyboard.Prose

-- | The Environment is a grab-bag of (scoped) constants.

data Environment = Environment
  { theProseStyle     :: TheProseStyle
  , theAlignment       :: Alignment -- ^ What alignment method are we using, left
--  ,  theBaselineOffset  :: Float     -- ^ size of offset from baseline, 0
--  ,  theDevicePixelRatio :: Float     -- scaling of whole page; Device Pixel Ratio.
  ,  theLineWidth   :: Float     -- ^ line width, 1
  , theLeftMargin   :: Float     -- ^ left margin, 0
  , theRightMargin  :: Float     -- ^ right margin, 0
  , theTabSize      :: Float     -- ^ indent size, 50
  , theParSkip      :: Float     -- ^ Space before each paragraph, 10
  }
  deriving Show

defaultEnvironment :: Environment
defaultEnvironment = Environment
  { theProseStyle      = defaultProseStyle
  , theAlignment       = left
--  ,  theBaselineOffset  = 0
--  ,  theDevicePixelRatio :: Float     -- scaling of whole page; Device Pixel Ratio.
  ,  theLineWidth       = 1
  , theLeftMargin       = 0
  , theRightMargin      = 0
  , theTabSize          = 50
  , theParSkip          = 10
  }


class Layout a where
  scoped :: (Environment -> Environment) -> a -> a

instance ProseStyle Environment where
  proseStyle f e = e { theProseStyle = f (theProseStyle e) }


indent :: Layout a => a -> a
indent = scoped $ \ cxt -> cxt { theLeftMargin = theLeftMargin cxt + theTabSize cxt }

align :: Layout a => Alignment -> a -> a
align j = scoped $ \ m -> m { theAlignment = j }

parskip :: Layout a => Float -> a -> a
parskip n = scoped $ \ m -> m { theParSkip = n }
