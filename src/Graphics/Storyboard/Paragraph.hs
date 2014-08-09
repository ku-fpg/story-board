{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Paragraph where

import Data.Monoid
import Data.Text(Text)

import Graphics.Storyboard.Literals
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Prelude
import Graphics.Storyboard.Prose
import Graphics.Storyboard.Tile

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

renderParagraph :: TheParagraphStyle -> Float -> Prose -> Prelude (Tile ())
renderParagraph par_style w ps = do
  let prose_style = theProseStyle par_style

  tiles <- renderProse (theAlignment par_style)
                       w
                       prose_style
                       ps

  -- Todo. Add decorations

  return $ pack $ mconcat $ map (anchor top) $ tiles

leftMargin :: ParagraphStyle a => Float -> a -> a
leftMargin n = paragraphStyle $ \ m -> m { theLeftMargin = n }
