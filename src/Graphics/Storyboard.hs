{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs, StandaloneDeriving, TypeFamilies, DataKinds #-}
module Graphics.Storyboard where

import Graphics.Blank hiding (eval)
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Data.List
import Data.Maybe

import Graphics.Storyboard.Types
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling
import Graphics.Storyboard.Markup
import Graphics.Storyboard.Images

-----------------------------------------------------------------------------

example1 :: Float -> Text -> Tile ()
example1 size col = id
    $ border 10 "black"
    $ tile (size,size) $ \ sz@(w,h) -> do

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


example2 :: Mosaic ()
example2 =
  anchor top gap *>
{-
  (anchor top $
   layoutLine (500,50)
              "10px sans-serif"
              3
              left
              [(Word [] "Hello",23),(Word [] "World",26)])
                                                *>
  (anchor top $ example1 100 "red")    *>
-}
{-
  (anchor top $ column [ example1 (i ^ 2 * 10) "red" | i <- [0..7]]) *>
  (anchor top $ example1 100 "green")  *>
  (left $ example1 100 "pink")    *> left gap *>
  (left$ example1 100 "blue")  *>
  (anchor top $ example1 200 "orange")  *>
  (left $ example1 100 "pink")    *>
  (left$ example1 100 "blue")  *> left gap *>
  (left $ example1 100 "pink")    *>
-}
  (anchor left$ example1 100 "blue")  *>
  (anchor right$ example1 100 "#123456")  *>
  pure ()


-- blank margin around a story.
margin :: Float -> Story a -> Story a
margin m inside = do
  storyMosaic (anchor top    (blankTile (0,m)))
  storyMosaic (anchor bottom (blankTile (0,m)))
  storyMosaic (anchor left   (blankTile (m,0)))
  storyMosaic (anchor right  (blankTile (m,0)))
  inside

-- horizontal rule
hr :: Story ()
hr = do
  (_,w) <- storyCavity
  storyMosaic $ anchor top $ tile (w,2) $ \ (w',h') -> do
              beginPath()
              moveTo(0,1)
              lineTo(w',0)
              lineWidth 2
              strokeStyle "black"
              stroke()

(</>) :: Prose -> Prose -> Prose
(</>) = (<+>)

titlePage :: Story ()
titlePage = margin 20 $ do
--  align center $ p $ "EECS 776"
  p $ "Functional Programming" </> "and Domain Specific Languages"
  p $ "Andy Gill"
  p $ "University of Kansas"
  p $ "August 26th 2013"  -- fix super

example3 :: Story ()
example3 = margin 20 $ do
  p $ "The Canvas monad forms a JavaScript/Canvas DSL, and we, where possible," <+>
      "stick to the JavaScript idioms. So a method call with no arguments takes a" <+>
      "unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of"

-- imageTile :: FilePath -> Story (Tile ())

  storyContext (justify Justified . spaceWidthX (* 1)) $ do
    p $ "Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
      "as needed (it turns out that all var-args functions take a variable number" <+>
      "of JavaScript numbers.)" <+>
      "unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of" <+>
      "Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
      "as needed (it turns out that all var-args functions take a variable number" <+>
      "of JavaScript numbers.)"

  hr

  img <- imageTile "jhwk_LF_200px.gif"

  storyMosaic (anchor left img)
  storyMosaic (anchor left (blankTile (20,0)))

  storyContext (justify Justified . spaceWidthX (* 1)) $ do

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


storyBoard :: Story a -> Canvas a
storyBoard story = do
    context <- myCanvasContext
    let cxt = MarkupContext "sans-serif" 32 (2.6 * 3.2) "black" left
    let cxt = MarkupContext "serif" 32 (2.6 * 3.2) "black" left
    let cxt = MarkupContext "Gill Sans" 32 (2.6 * 3.2) "black" left
    (a,mosaic) <- runPrelude $ runStory story cxt (width context,height context)
    let Tile (w,h) m = fillTile mosaic
    saveRestore $ do
      _ <- m (w,h)
      return ()
    return a

main = blankCanvas 3000 $ \ context -> do
      send context $ do
        storyBoard titlePage
