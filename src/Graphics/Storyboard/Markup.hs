{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Markup where

import qualified Data.Text as Text
import Data.Text(Text)
import Data.Semigroup
import qualified Graphics.Blank as Blank
import Graphics.Blank(Canvas,fillStyle,fillText,saveRestore,measureText,TextMetrics(..))
import Data.List as List
import Control.Applicative

import Graphics.Storyboard.Slide
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Literals
import Graphics.Storyboard.Prose
import Graphics.Storyboard.Paragraph
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Bling
import Graphics.Storyboard.Prelude

import Control.Monad.IO.Class

------------------------------------------------------------------------


------------------------------------------------------------------------

-- | build a tile around a word, but do not place it.

word :: Text -> Slide (Tile ())
word txt = slide $ \ cxt (w,h) -> undefined
{-
    let ps_cxt = theProseStyle cxt
    w <- wordWidth cxt (Word [] txt)
    return ( tile (w,fromIntegral $ theFontSize ps_cxt + 5) $ const $ do
        Blank.font $ emphasisFont (theFontSize ps_cxt) (theFont ps_cxt) []
        fillStyle (theColor ps_cxt)
        fillText (txt,0,fromIntegral $ theFontSize ps_cxt), pure ())
-}
------------------------------------------------------------------------

{-
item :: Text -> Prose -> Slide ()
item txt prose = do
  cxt <- environment
  t <- word txt
  draw (pack ((blank (theLeftMargin cxt,0) ? left) <> (point top right t ? left)) ? top)
  p prose
-}

indent = id

p :: Prose -> Slide ()
p ps = slide $ \ slide_style (w,h) -> do
    let par_style = theParagraphStyle slide_style

    t <- renderParagraph par_style w ps

    return ((),anchor top t)

------------------------------------------------------------------------
