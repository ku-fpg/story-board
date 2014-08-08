{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Environment where

import Data.Text(Text)

import Graphics.Storyboard.Literals
import Graphics.Storyboard.Prose

-- | The Environment is a grab-bag of (scoped) constants.

data TheParagraphStyle = TheParagraphStyle
  { theProseStyle   :: TheProseStyle
  , theAlignment    :: Alignment -- ^ What alignment method are we using, left
  , theLineWidth    :: Float     -- ^ line width, 1
  , theLeftMargin   :: Float     -- ^ left margin, 0
  , theRightMargin  :: Float     -- ^ right margin, 0
  , theTopMargin    :: Float     -- ^ top margin, 0
  , theBottomMargin :: Float     -- ^ bottom margin, 0
  }
  deriving Show

defaultParagraphStyle :: TheParagraphStyle
defaultParagraphStyle = TheParagraphStyle
  { theProseStyle     = defaultProseStyle
  , theAlignment      = left
  , theLineWidth      = 1
  , theLeftMargin     = 0
  , theRightMargin    = 0
  , theTopMargin      = 0
  , theBottomMargin   = 0
  }


instance ProseStyle TheParagraphStyle where
  proseStyle f e = e { theProseStyle = f (theProseStyle e) }

class ProseStyle a => ParagraphStyle a where
  paragraphStyle :: (TheParagraphStyle -> TheParagraphStyle) -> a -> a

instance ParagraphStyle TheParagraphStyle where
  paragraphStyle f s = f s

align :: ParagraphStyle a => Alignment -> a -> a
align j = paragraphStyle $ \ m -> m { theAlignment = j }
