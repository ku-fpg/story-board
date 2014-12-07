{-# LANGUAGE ScopedTypeVariables, KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Paragraph where

import Data.Text (Text)
import Data.Monoid

import Graphics.Storyboard.Literals
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Prelude
import Graphics.Storyboard.Prose
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Types

-- | The Environment is a grab-bag of (scoped) constants.

data TheParagraphStyle = TheParagraphStyle
  { theAlignment    :: Alignment -- ^ What alignment method are we using, left
  , theLineWidth    :: Double     -- ^ line width, 1
  , theLeftMargin   :: Double     -- ^ left margin, 0
  , theRightMargin  :: Double     -- ^ right margin, 0
  , theTopMargin    :: Double     -- ^ top margin, 0
  , theBottomMargin :: Double     -- ^ bottom margin, 0
  , theBullet       :: Maybe Bullet
  }
  deriving Show

-- For the Bullet, make make the top *right* corner the hotspot,
-- and connect it with the top *left* of the paragraph.
newtype Bullet = Bullet { runBullet :: TheProseStyle -> Prelude (Tile ()) }

instance Show Bullet where
  show (Bullet _) = "Bullet{}"

defaultParagraphStyle :: TheParagraphStyle
defaultParagraphStyle = TheParagraphStyle
  { theAlignment      = left
  , theLineWidth      = 1
  , theLeftMargin     = 0
  , theRightMargin    = 0
  , theTopMargin      = 0
  , theBottomMargin   = 0
  , theBullet         = Nothing
  }


class ParagraphStyle a where
  paragraphStyle :: (TheParagraphStyle -> TheParagraphStyle) -> a -> a

instance ParagraphStyle TheParagraphStyle where
  paragraphStyle f s = f s

align :: ParagraphStyle a => Alignment -> a -> a
align j = paragraphStyle $ \ m -> m { theAlignment = j }

renderParagraph :: TheProseStyle -> TheParagraphStyle -> Double -> Prose -> Prelude (Tile ())
renderParagraph prose_style par_style w ps = do

  tiles <- renderProse (theAlignment par_style)
                       (w - (theLeftMargin par_style + theRightMargin par_style))
                       prose_style
                       ps

  -- Todo. Add decorations

  bullet_mosaic :: Mosaic () <- case theBullet par_style of
            Nothing -> return mempty
            Just (Bullet f)-> do
                r <- f prose_style
                return (anchor left $ point top right $ r)

  return $ pack $ mconcat $
              [ anchor left $ blank (theLeftMargin par_style,0)
              ] ++
              [ bullet_mosaic ] ++

              map (anchor top) tiles

leftMargin :: ParagraphStyle a => Double -> a -> a
leftMargin n = paragraphStyle $ \ m -> m { theLeftMargin = n }

rightMargin :: ParagraphStyle a => Double -> a -> a
rightMargin n = paragraphStyle $ \ m -> m { theRightMargin = n }

bullet :: ParagraphStyle a => Bullet -> a -> a
bullet b' = paragraphStyle $ \ m -> m { theBullet = Just b' }

noBullet :: ParagraphStyle a => a -> a
noBullet = paragraphStyle $ \ m -> m { theBullet = Nothing }


renderMargin :: TheParagraphStyle -> Mosaic ()
renderMargin par_style = mconcat
    [ anchor left $ blank (theLeftMargin par_style,0)
    ]


theParagraphMarginSize :: TheParagraphStyle -> Size Double
theParagraphMarginSize par_style =
    (theLeftMargin par_style + theRightMargin par_style, 0)


-------------------------------------------------------------------------------

bulletText :: Text -> Bullet
bulletText txt = Bullet $ \ prose_st -> renderText prose_st txt
