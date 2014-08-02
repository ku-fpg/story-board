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


example2 :: Filler ()
example2 =
  top gap *>
{-
  (top $
   layoutLine (500,50)
              "10px sans-serif"
              3
              JustLeft
              [(Word [] "Hello",23),(Word [] "World",26)])
                                                *>
  (top $ example1 100 "red")    *>
-}
{-
  (top $ column [ example1 (i ^ 2 * 10) "red" | i <- [0..7]]) *>
  (top $ example1 100 "green")  *>
  (left $ example1 100 "pink")    *> left gap *>
  (left$ example1 100 "blue")  *>
  (top $ example1 200 "orange")  *>
  (left $ example1 100 "pink")    *>
  (left$ example1 100 "blue")  *> left gap *>
  (left $ example1 100 "pink")    *>
-}
  (left$ example1 100 "blue")  *>
  (right$ example1 100 "#123456")  *>
  pure ()

txt :: Prose
txt =
  "The Canvas monad forms a JavaScript/Canvas DSL, and we, where possible," <+>
  "stick to the JavaScript idioms. So a method call with no arguments takes a" <+>
  "unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of" <+>
  "Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
  "as needed (it turns out that all var-args functions take a variable number" <+>
  "of JavaScript numbers.)"


main = blankCanvas 3000 $ \ context -> do
      send context $ do
        let cxt = MarkupContext "sans-serif" 32 (3 * 3.2) "black" JustLeft 470
        filler <- tileProse cxt txt
        let Tile (w,h) m = border 1 "red" $ fillTile filler
        saveRestore $ do
          translate (10,100)
          _ <- m (w,h)
          return ()

        let cxt = MarkupContext "sans-serif" 32 (3 * 3.2) "black" Justified 470
        filler <- tileProse cxt txt
        let Tile (w,h) m = border 1 "red" $ fillTile filler
        saveRestore $ do
          translate (550,100)
          _ <- m (w,h)
          return ()

        return ()

{-
      send context $ do
        fillStyle "orange"
        translate (0,0)
        let Tile (w,h) m = fillTile example2
        _ <- m (w + 100,h + 100)
        return ()
-}
