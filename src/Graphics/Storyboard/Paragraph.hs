{-# LANGUAGE ScopedTypeVariables, KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

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
  , theBullet       :: Maybe Bullet
  }
  deriving Show

-- For the Bullet, make make the top *right* corner the hotspot,
-- and connect it with the top *left* of the paragraph.
newtype Bullet = Bullet { runBullet :: TheProseStyle -> Prelude (Tile ()) }

instance Show Bullet where
  show (Bullet t) = "Bullet{}"

defaultParagraphStyle :: TheParagraphStyle
defaultParagraphStyle = TheParagraphStyle
  { theProseStyle     = defaultProseStyle
  , theAlignment      = left
  , theLineWidth      = 1
  , theLeftMargin     = 0
  , theRightMargin    = 0
  , theTopMargin      = 0
  , theBottomMargin   = 0
  , theBullet         = Nothing
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
                       (w - (theLeftMargin par_style + theRightMargin par_style))
                       prose_style
                       ps

  -- Todo. Add decorations

  bullet_mosiac :: Mosaic () <- case theBullet par_style of
            Nothing -> return mempty
            Just (Bullet f)-> do
                r <- f prose_style
                return (anchor left $ point top right $ r)


  return $ pack $ mconcat $
              [ anchor left $ blank (theLeftMargin par_style,0)
              ] ++
              [ bullet_mosiac ] ++
{-
              [ anchor left $ point top right $ bull prose_style
              | Just (Bullet bull) <- [theBullet par_style]
              ] ++
-}
              map (anchor top) tiles

leftMargin :: ParagraphStyle a => Float -> a -> a
leftMargin n = paragraphStyle $ \ m -> m { theLeftMargin = n }

bullet :: ParagraphStyle a => Bullet -> a -> a
bullet b = paragraphStyle $ \ m -> m { theBullet = Just b }

noBullet :: ParagraphStyle a => a -> a
noBullet = paragraphStyle $ \ m -> m { theBullet = Nothing }

-------------------------------------------------------------------------------

bulletText :: Text -> Bullet
bulletText txt = Bullet $ \ prose_st -> renderText prose_st txt
