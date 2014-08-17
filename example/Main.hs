{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.Blank hiding (font)

import Graphics.Storyboard

import Graphics.Storyboard.Act
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Box
import Graphics.Storyboard.Types


main :: IO ()
main = storyBoard $ map brand
  [ haskell_code
  , titleSlide
  , bigBackground
  , alignSlide
  , bulletSlide
  , orderedListSlide
  , actSlide
  ]

brand :: Slide () -> Slide ()
brand slide = font "Gill Sans" $ do
  img <- imageTile "KUlogo1C.png"
  place right $ pack $ anchor bottom $ point bottom right $ img
  t <- tileOfSlide (200,20) $ fontSize 10 $ p $ " \xa9" <> "2014 Andrew Gill" -- tiny
  place bottom $ point bottom left $ t
  margin 20 $ fontSize 20 $ slide

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
  fontSize 18 $ p $ "\xa9" <+> "2014 Andrew Gill"


bigBackground :: Slide ()
bigBackground = do
  img <- imageTile "KU.png"
  place left $ point top left $ img

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
    pause
    li $ lorem
    pause
    li $ lorem
    pause
    li $ lorem
    pause
    li $ lorem
    pause
    li $ lorem

orderedListSlide :: Slide ()
orderedListSlide = margin 20 $ fontSize 20 $ font "Gill Sans" $ do

  ol $ do
    li $ lorem
    li $ lorem
    li $ lorem

{-
slide_background :: Slide ()
slide_background = do
  img <- imageTile "KUlogo1C.png"
  place right $ pack $ anchor bottom $ point bottom right $ img

  t <- tileOfSlide (200,20) $ do
    fontSize 10 $ p $ " \xa9" <> "2014 Andrew Gill"  -- tiny

  place bottom $ point bottom left $ t
-}

{-
  t <- tileOfSlide (200,20) $ do
    p $ " \xa9" <> "2014 Andrew Gill"
    place bottom (box defaultBoxStyle t)

  -- draw (vbrace h <> hbrace w)

  img <- imageTile "KUlogo1C.png"
  let img' = mapTileAct (\ act -> do
              liftCanvas (save() >> globalAlpha 0.3)
              () <- act
              liftCanvas (restore())
              return ()) img
  place right (pack (anchor bottom img'))
  t <- tileOfSlide (200,20) $ do
    p $ " \xa9" <> "2014 Andrew Gill"
  place bottom (box defaultBoxStyle t)
-}
haskell_code :: Slide ()
haskell_code = margin 20 $ fontSize 20 $ font "Gill Sans" $ do
  txt <- liftIO $ readFile "example/Main.hs"
  liftIO $ print $ highlight defaultHighlightStyle
                $ unlines
                $ take 15
                $ drop 100
                $ map (take 72)
                $ lines
                $ txt


  lg <- bgLinear "red" "blue"
  font "Courier New" $ fontSize 28 $ trueSpace $ do
    background lg $ frame $ margin 10 $ do
      p $ highlight defaultHighlightStyle
            $ unlines
            $ take 15
            $ drop 100
            $ map (take 72)
            $ lines
            $ txt

{-
--  let ft = "monospace"
  let ft = "sans-serif"
--  let ft = "serif"

  sequence [ do t1 <- tileOfSlide (1000,0) $ do
                  fontSize n $ font ft $ align center $ p $ "Object.prototype WA"
                place top $ (border 1 "black" t1)
           | n <- reverse $ take 12 $ map ceiling $ iterate (* 1.2) (10 :: Float)
           ]

  return ()
-}

{-
  let border = box defaultBoxStyle
  let border2 = box defaultBoxStyle { theBackground = LinearGradient "blue" "red" }


  t1 <- tileOfSlide (202,0) $ do
        font "sans-serif" $ align center $ p $ "Object.prototype"
  t2 <- tileOfSlide (202,30) $ do
        font "monospace" $ align center $ p $ "<builtins>"
  t3 <- tileOfSlide (100,30) $ do
        font "monospace" $ align center $ p $ "__proto__"
  t4 <- tileOfSlide (100,0) $ return ()

  t5 <- tileOfSlide (100,0) $ do
      font "monospace" $ align center $ p $ "__proto__"

  liftIO $ print $ map tileSize [t1,t2,t3,t4,t5]

  place left $ border t1

  place top $ pack $
    border2 t1 ? left
  place top $ pack $
    border t2 ? left
  place top $ pack $
    border t5 ? left <>
    border t4 ? left
-}

--  draw (gap bottom)

---  draw (gap left <> anchor bottom img)

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

actSlide :: Slide ()
actSlide = margin 20 $ fontSize 20 $ font "Gill Sans" $ do
  fontSize 72 $ p $ "The Act"

  place top $ tile (100,100) $ \ (x,y) (w,h) -> do
    replay (0,100) $ \ n ->
          saveRestore $ do
            translate(x,y)
            clearRect(0,0,w,h)
            beginPath()
            rect(1,1,w-(fromIntegral n*10+2),h-2)
            strokeStyle "red"
            lineWidth 1
            stroke()
            closePath()

{-
  lg <- bgLinear "yellow" "white"
  table
    [ tr [ background lg $
            td $ font "sans-serif" $ align center $ p $ prose $ "X " ++ show (n,m)
         | n <- [1..10], n <= m
         ]
    | m <- [1..10]
    ]
  ol $ do
    li $ lorem
    p $ lorem
  indent $ frame $ do
    p $ "Hello"
    ul $ do
      li $ lorem
      p $ lorem
-}
