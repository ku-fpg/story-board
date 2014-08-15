{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.Blank hiding (font)

import Graphics.Storyboard

import Graphics.Storyboard.Act
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Types


main :: IO ()
main = storyBoard $
  [ haskell_code
  , slide_background
  , titleSlide
  , alignSlide
  , bulletSlide
  , orderedListSlide
  , actSlide
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
  fontSize 18 $ p $ "\xa9" <+> "2014 Andrew Gill"


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


slide_background :: Slide ()
slide_background = margin 10 $ fontSize 16 $ do
  (w,h) <- getCavitySize
  draw (vbrace h <> hbrace w)
--  img <- imageTile "jhwk_LF_200px.gif"

  table
    [ tr [ background (LinearGradient "yellow" "white") $
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


haskell_code :: Slide ()
haskell_code = do
  txt <- liftIO $ readFile "example/Main.hs"
--  liftIO $ print $ highlight haskellHighlightStyle txt
  font "Courier New" $ fontSize 28 $ trueSpace $
    p $ highlight haskellHighlightStyle (take 2000 txt)

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

{-
grd # addColorStop(0, "#8EA154")
-- dark blue
grd # addColorStop(1, "#7D8E4B")

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
    let loop n = do
          liftIO $ print n
          liftCanvas $ saveRestore $ do
            translate(x,y)
            clearRect(0,0,w,h)
            beginPath()
            rect(1,1,w-(n+2),h-2)
            strokeStyle "red"
            lineWidth 1
            stroke()
            closePath()
          nextAnimationFrame $ loop (n+1)
    loop 0
