{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.Monoid

import Graphics.Storyboard

main :: IO ()
main = storyBoard $ drop 1 [alignSlide, example1]


lorem :: Prose
lorem =
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In interdum," <+>
      "nisl sit amet gravida placerat, felis odio rutrum justo, sed viverra" <+>
      "lacus erat at orci. Mauris eu nibh vitae dolor venenatis aliquam vel" <+>
      "ac metus. Praesent hendrerit enim purus, et blandit eros sagittis" <+>
      "eu. Phasellus dolor est, tempor a fermentum in, fringilla id" <+>
      "turpis. Fusce orci quam, pulvinar sed felis sed, euismod mollis" <+>
      "metus. Aliquam aliquet massa quam, ac scelerisque augue malesuada" <+>
      "ut. Suspendisse tincidunt nulla non molestie vestibulum."

ligatures :: Prose
ligatures = "ff fi fl ffi ffl"
file (\xfb00) (\xfb01) (\xfb03)"

alignSlide :: Slide ()
alignSlide = margin 20 $ fontSize 20 $ font "Gill Sans" $ do

  p $ lorem

  align right     $ p $ lorem
  align center    $ p $ lorem
  align justified $ p $ lorem

example1 :: Slide ()
example1 = margin 20 $ fontSize 20 $ font "Gill Sans" $ do

  p $ lorem

  img <- imageTile "jhwk_LF_200px.gif"

  draw (border 3 "orange" (pack (img ?left <> gap left <> img ?left <> gap left <> img ? right)) ? top)

  fontSize 72 $ p $ ligatures


{-
  indent $ do
    item "\x2022 " $ lorem
    item "\x2022 " $ lorem
    item "\x2022 " $ lorem
-}
--  draw (border 1 "red" t ? top)
---  let pic = (colorTile "blue" (100,100) ? left) <> (border 1 "orange" (point bottom right t) ? left) <> gap left
--  draw (border 1 "green" (pack pic) ? top)
