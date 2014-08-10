{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Control.Monad.IO.Class
import Data.Monoid

import Graphics.Storyboard

main :: IO ()
main = storyBoard $ take 1 $ drop 0
  [ titleSlide
  , alignSlide
  , bulletSlide
  , orderedListSlide
  ]


titleSlide :: Slide ()
titleSlide = margin 20 $ fontSize 20 $ font "Gill Sans" $ align center $ do
  fontSize 72 $ p $ "The Storyboard DSL"
  vspace 28
  fontSize 48 $ p $ "(a.k.a. beamer in Haskell)"
  vspace 100
  fontSize 28 $ p $ "Andy Gill"
  vspace 28
  fontSize 24 $ p $ "The University of Kansas"
  vspace 100
  fontSize 18 $ p $ "Copyright\xa9" <+> "2014 Andrew Gill"


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

alignSlide :: Slide ()
alignSlide = margin 20 $ fontSize 20 $ font "Gill Sans" $ do

  p $ lorem
  align right     $ p $ lorem
  align center    $ p $ lorem
  align justified $ p $ lorem


bulletSlide :: Slide ()
bulletSlide = margin 20 $ fontSize 20 $ font "Gill Sans" $ do

  ul $ do
    li $ lorem
    li $ lorem
    li $ lorem

orderedListSlide :: Slide ()
orderedListSlide = margin 20 $ fontSize 20 $ font "Gill Sans" $ do

  ol $ do
    li $ lorem
    li $ lorem
    li $ lorem

{-
example1 :: Slide ()
example1 = margin 20 $ fontSize 20 $ font "Gill Sans" $ do

  p $ lorem

  img <- imageTile "jhwk_LF_200px.gif"

  draw (border 3 "orange" (pack (img ?left <> gap left <> img ?left <> gap left <> img ? right)) ? top)

  fontSize 72 $ p $ ligatures

-}
{-
  indent $ do
    item "\x2022 " $ lorem
    item "\x2022 " $ lorem
    item "\x2022 " $ lorem
-}
--  draw (border 1 "red" t ? top)
---  let pic = (colorTile "blue" (100,100) ? left) <> (border 1 "orange" (point bottom right t) ? left) <> gap left
--  draw (border 1 "green" (pack pic) ? top)
