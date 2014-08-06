{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class

import Graphics.Storyboard

main :: IO ()
main = storyBoard [example3]

example3 :: Story ()
example3 = margin 20 $ do
  p $ "The Canvas monad forms a JavaScript/Canvas DSL, and we, where possible," <+>
      "stick to the JavaScript idioms. So a method call with no arguments takes a" <+>
      "unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of"

  (w,h) <-storyCavity
  liftIO $ print (w,h)

-- imageTile :: FilePath -> Story (Tile ())

  align justified $ do
    p $ "FXoats, etc. When there is a var-args JavaScript function, we use lists," <+>
      "as needed (it turns out that all var-args functions take a variable number" <+>
      "of JavaScript numbers.)"


  itemize $ do
    p $ "FF unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of" <+>
      "Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
      "as needed (it turns out that all var-args functions take a variable number" <+>
      "of JavaScript numbers.)"

  hr

  img <- imageTile "jhwk_LF_200px.gif"

  draw (anchor left img)
  draw (anchor left (blank (20,0)))

  align justified $ do
    p $ " Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
        "as needed (it turns out that all var-args functions take a variable number" <+>
        "of JavaScript numbers.)" <+>
        "unit, a method call that takes 3 JavaScript numbers will take a 3-tuple of" <+>
        "Floats, etc. When there is a var-args JavaScript function, we use lists," <+>
        "as needed (it turns out that all var-args functions take a variable number" <+>
        "of JavaScript numbers.)"
