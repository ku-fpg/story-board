{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs, StandaloneDeriving, TypeFamilies, DataKinds #-}
module Graphics.Storyboard
  ( Slide
    -- * Markup
  , p
  , ul
  , ol
  , li
    -- * Spacing
  , vspace
  , (<+>)
  , align
  , getCavitySize
  , imageTile
  , draw
  , anchor
  , blank
  , storyBoard
  , indent
  , margin
  , hr
  , Prose
  , prose
    -- * Tiles
  , tile
  , tileSize
  , tileWidth
  , tileHeight
    -- Tables
  , tr, td, table
  , background
  , frame
    -- * Mosaic
  , hbrace
  , vbrace
    -- * Highlights
  , highlight
  , defaultHighlightStyle
  , haskellHighlightStyle
    -- * Useful literals
  , module Graphics.Storyboard.Literals
  , font, fontSize
--  , word
  , (?)
  , border
  , point
  , colorTile
  , gap
  , pack
--  , environment
--  , item
  , tileOfSlide
  , place
  , module Graphics.Storyboard.Box

  )


where

import Graphics.Blank hiding (eval,font)
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Data.List
import Data.Maybe
import Control.Monad.IO.Class

import Graphics.Storyboard.Slide
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling
import Graphics.Storyboard.Highlight
import Graphics.Storyboard.Markup
import Graphics.Storyboard.Types
import Graphics.Storyboard.Images
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Literals
import Graphics.Storyboard.Prose
import qualified Graphics.Storyboard.Prelude as Prelude
import Graphics.Storyboard.Paragraph
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Box

-----------------------------------------------------------------------------
{-
example1 :: Float -> Text -> Tile ()
example1 sz col = id
    $ border 10 "black"
    $ tile (sz,sz) $ \ sz@(w,h) -> do

        -- background
        beginPath()
        strokeStyle "#dddddd"
        rect(0,0,w,h)
        closePath()
        stroke()

        -- assumes 100 x 100 pixels sized viewport
        saveRestore $ do
{-
          shadowColor "black"
          shadowOffsetX 5
          shadowOffsetY 5
          shadowBlur 5
-}
          beginPath()
          fillStyle col
          arc(size/2, size/2, (size / 2.5), 0, pi*2, False)
          closePath()
          fill()
-}


-- blank margin around a story.
margin :: Float -> Slide a -> Slide a
margin m inside = do
  draw (blank (0,m) ?top)
  draw (anchor bottom (blank (0,m)))
  draw (anchor left   (blank (m,0)))
  draw (anchor right  (blank (m,0)))
  inside

-- horizontal rule
hr :: Slide ()
hr = do
  (_,w) <- getCavitySize
  draw $ anchor top $ tile (w,2) $ \ (w',h') -> do
              beginPath()
              moveTo(0,1)
              lineTo(w',0)
              lineWidth 2
              strokeStyle "black"
              stroke()

vspace :: Float -> Slide ()
vspace h = do
  draw $ anchor top $ blank (0,h)

titlePage :: Slide ()
titlePage = margin 20 $ align center $ do
--  align center $ p $ "EECS 776"
  fontSize 72 $ p $ "Functional Programming" </> "and Domain Specific Languages"
  vspace 100
  fontSize 28 $ p $ "Andy Gill"
  fontSize 20 $ p $ "University of Kansas"
  vspace 100
  fontSize 18 $ p $ "August 26" <> super "th" <+> "2013"  -- fix super
  vspace 100
  fontSize 18 $ p $ "Copyright" <> "\xa9" <+> "2014 Andrew Gill"

--bullet :: Slide ()
--bullet = "*"

{-
background :: Slide a -> Slide a
background (Slide bg) = do

overlay :: Monoid a => Slide a -> Slide a -> Slide a
overlay (Slide storyA) (Slide storyB) = Slide $ \ cxt sz -> do
    (a,mA) <- storyA cxt sz
    (b,mB) <- storyB cxt sz
    return (a <> b,
-}

slide_background :: Slide ()
slide_background = margin 10 $ do
  (w,h) <-getCavitySize
  draw (vbrace h <> hbrace w)
  img <- imageTile "jhwk_LF_200px.gif"

  draw (gap left <> anchor bottom img)


example3 :: Slide ()
example3 = margin 20 $ do
  align justified $ do

    p $ "The Canvas monad forms a JavaScript/Canvas DSL, and we, where possible," <+>
        "stick to the JavaScript idioms. So a method call with no arguments takes a" <+>
        "unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of"

  pause

  align justified $ do
    p $ "FXoats, etc. When there is a var-args JavaScript function, we use lists," <+>
      "as needed (it turns out that all var-args functions take a variable number" <+>
      "of JavaScript numbers.)"

  pause

  indent $ align right $ do
    p $ "FF unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of" <+>
      "Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
      "as needed (it turns out that all var-args functions take a variable number" <+>
      "of JavaScript numbers.)"

  pause

  indent $ align left $ do
    p $ "FF unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of"

  hr

  pause

  img <- imageTile "jhwk_LF_200px.gif"

  draw (anchor left img)
  draw (anchor left (blank (20,0)))

  pause

  align justified $ do
    p $ " Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
        "as needed (it turns out that all var-args functions take a variable number" <+>
        "of JavaScript numbers.)" <+>
        "unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of" <+>
        "Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
        "as needed (it turns out that all var-args functions take a variable number" <+>
        "of JavaScript numbers.)"


txt :: Prose
txt =
  "The Canvas monad forms a JavaScript/Canvas DSL, and we, where possible," <+>
  "stick to the JavaScript idioms. So a method call with no arguments takes a" <+>
  "unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of" <+>
  "Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
  "as needed (it turns out that all var-args functions take a variable number" <+>
  "of JavaScript numbers.)"


blankCanvasStoryBoard :: [Slide ()] -> DeviceContext -> IO ()
blankCanvasStoryBoard slide = \ context -> send context $ do
    let cxt = defaultSlideStyle (width context,height context)
    let st0 = defaultSlideState (fullSize cxt)
    (_,st1) <- Prelude.runPrelude (runSlide (head slide) cxt st0) (eventQueue context)
    let Tile (w,h) m = fillTile (theMosaic st1)
    saveRestore $ do
      _ <- m (w,h)
      return ()
    return ()

storyBoard :: [Slide ()] -> IO ()
storyBoard = blankCanvas 3000 { middleware = [], events = ["keypress"] }
           . blankCanvasStoryBoard

main :: IO ()
main = storyBoard [example3]
