{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Markup where

import           Control.Applicative

import           Data.Semigroup
import qualified Data.Text as Text

import           Graphics.Storyboard.Slide
import           Graphics.Storyboard.Tile
import           Graphics.Storyboard.Literals
import           Graphics.Storyboard.Prose
import           Graphics.Storyboard.Paragraph
import           Graphics.Storyboard.Mosaic
import           Graphics.Storyboard.Box

------------------------------------------------------------------------


------------------------------------------------------------------------

-- | build a tile around a word, but do not place it.

--word :: Text -> Slide (Tile ())
--word txt = slide $ \ cxt (w,h) -> undefined
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


indent :: Slide a -> Slide a
indent m = do
  i' <- theItemCounter <$> getSlideState
  modSlideState (setItemCount 0)
  r <- consItemCounters i' $ do
      indLevel <- length <$> theItemCounters <$> askSlideStyle
      tabStop <- theTabStop <$> askSlideStyle
      leftMargin (fromIntegral indLevel * tabStop) $ m
  modSlideState (setItemCount i')
  return r

--  incIndentLevel $
--  incLevel <- length <$> theSectionCount <$> getSlideState
--  tabStop <- theTabStop <$> askSlideStyle
--  leftMargin (fromIntegral indLevel * tabStop) m
--s  m

ul :: Slide a -> Slide a
ul = indent
   . bulletFactory defaultBulletFactory

ol :: Slide a -> Slide a
ol = indent
   . bulletFactory (BulletFactory $ \ i' _ -> bulletText (Text.pack (show i') <> ". "))

-- t <- slidePrelude $ renderText par_st "\x2022 "

li :: Prose -> Slide ()
li ps = do
  modSlideState incItemCount
  i' <- theItemCounter <$> getSlideState
  is <- theItemCounters <$> askSlideStyle
  _  <- theProseStyle <$> askSlideStyle
  BulletFactory fac <- theBulletFactory <$> askSlideStyle
  bullet (fac i' is) $ p ps

p :: Prose -> Slide ()
p ps = do
    (w,_)       <- getCavitySize
    slide_style <- askSlideStyle
    let prose_style = theProseStyle slide_style
    let par_style = theParagraphStyle slide_style
    t <- slidePrelude $ renderParagraph prose_style par_style w ps
    place top t

------------------------------------------------------------------------

frame :: Slide () -> Slide ()
frame s = table [tr [td s]]

-- boxes :: TheBoxStyle -> [[(TheBoxStyle -> TheBoxStyle,Tile ())]] -> Tile ()

data TD = TD (TheBoxStyle -> TheBoxStyle) (Slide ())

instance BoxStyle TD where
  boxStyle f (TD g s) = TD (g . f) s

td :: Slide () -> TD
td = TD id

newtype TR = TR [TD]

tr :: [TD] -> TR
tr = TR

table :: [TR] -> Slide ()
table rows = do
  (w,_) <- getCavitySize

  paragraphStyle' <- theParagraphStyle <$> askSlideStyle
  let (w',_) = theParagraphMarginSize paragraphStyle'
  ln_wd <- theBorderWidth <$> theBoxStyle <$> askSlideStyle

  let gaps n = (w - w') / n - ln_wd * 2

  -- Right now, we divide the width evenly among all the elements in each row
  tss :: [[(TheBoxStyle -> TheBoxStyle, Tile ())]] <- sequence
      [ sequence [ do t <- tileOfSlide (gaps (fromIntegral (length tds)),0) s
                      return (f,t)
                 | TD f s <- tds
                 ]
      | TR tds <- rows
      ]
  boxStyle' <- theBoxStyle <$> askSlideStyle

  place top $ pack $ mconcat
      [ renderMargin paragraphStyle'
      , anchor top $ boxes boxStyle' tss
      ]

  return ()

-- code :: Text -> Story ()
