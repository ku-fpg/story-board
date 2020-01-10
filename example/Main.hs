{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.Monoid
import Graphics.Storyboard

main :: IO ()
main = storyBoard 3000 $ (sizeSlide :) $ map brand $ drop 0
  [ titleSlide
  , haskell_code
  , bigBackground
  , alignSlide
  , bulletSlide
  , orderedListSlide
  , actSlide
  , subSuperSlide
--  , cachingTiles
  , scalingTiles
  ]

brand :: Slide () -> Slide ()
brand slide = font "Gill Sans" $ do
  img <- imageTile "example/images/KUlogo1C.png"
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

sizeSlide :: Slide ()
sizeSlide = do
  (x,y) <- getCavitySize
  p $ prose $ "Slide cavity size: " ++ show (x,y)

bigBackground :: Slide ()
bigBackground = do
  img <- imageTile "example/images/KU.png"
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
  --let txt = "Hello, World\nPrelude> 1 + 2\n For 1 + 2 now"
  liftIO $ print $ highlight ghciHighlightStyle
                $ unlines
                $ take 15
--                $ drop 100
                $ map (take 72)
                $ lines
                $ txt


  lg <- bgLinear "red" "blue"
  font "Courier New" $ fontSize 28 $ trueSpace $ do
    background lg $ frame $ margin 10 $ do
      p $ highlight ghciHighlightStyle
            $ unlines
            $ take 15
--            $ drop 100
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
           | n <- reverse $ take 12 $ map ceiling $ iterate (* 1.2) (10 :: Double)
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
  fontSize 72 $ p $ "The Act datatype"
{-
  p $ "This box get smaller over time, then stops"

  place top $ tile (100,100) $ \ (x,y) (w,h) -> do
    actOnBehavior timerB (Cavity (x,y) (w,h)) $ \ n -> do
          let n' = min n 1
          saveRestore $ do
            translate(x,y)
            clearRect(0,0,w,h)
            beginPath()
            rect(1,1,w-(n'*1000+2),h-2)
            strokeStyle "red"
            lineWidth 1
            stroke()
            closePath()
            return (n >= 1)
{-
  p $ "This box gets smaller and larger over time"
  place top $ tile (100,100) $ \ (x,y) (w,h) -> do
    actOnBehavior timerB (Cavity (x,y) (w,h)) $ \ n -> do
          let n' = (1 + sin n) / 2
          saveRestore $ do
            translate(x,y)
--            clearRect(0,0,w,h)
            beginPath()
            rect(1,1,w-(n'*1000+2),h-2)
            strokeStyle "red"
            lineWidth 1
            stroke()
            closePath()
            return False
-}
  p $ "And the values"
  place top $ tile (100,100) $ \ (x,y) (w,h) -> do
      actOnBehavior ((,,) <$> timerB <*> eventB <*> cavityB)
             (Cavity (x,y) (w,h)) $ \ (tm,ev,ca) -> do
            saveRestore $ do
              translate(x,y)
    --            clearRect(0,0,w,h)
              beginPath()
              rect(1,1,w-2,h-2)
              strokeStyle "red"
              lineWidth 1
              stroke()
              closePath()
              fillText ("Hello" <> Text.pack (show (tm,ev,ca)),10,10)
              return False

  p $ "1"
  pause
  p $ "2"
  pause
  p $ "3"

-}
{-
  (t1,b) <- liftIO $ tileAddress (100,100)

  let f a b = case a of
                Just event | eType event == "mousemove" ->
                    case ePageXY event of
                      Just (x,y) -> (x,y)
                      _ -> b
                _ -> b
  mm <- liftIO $ atomically $ switch f (0,0) $ eventB


  let inside ((x,y),(w,h)) (w',h')
            | w' >= x && h' >= y && (w' - x) <= w && (h' - y) <= h
            = return (w'-x,h'-y)
      inside _ _ = Nothing

  let insideB :: Behavior (Maybe (Double,Double))
      insideB = pure inside
          <*> b
          <*> mm

  let t2 = tile (100,100) $ \ (x,y) (w,h) -> do
              actOnBehavior insideB $ \ n -> do
--                    liftIO $ print n
                    saveRestore $ do
                      translate(x,y)
                      clearRect(0,0,w,h)
                      beginPath()
                      rect(1,1,w-2,h-2)
                      strokeStyle (case n of
                                     Nothing -> "red"
                                     Just _ -> "green")
                      lineWidth 1
                      stroke()
                      closePath()

                      case n of
                        Nothing -> return ()
                        Just (x',y') -> do
                          beginPath()
                          arc(x', y', 10, 0, 2 * pi, False)
                          fillStyle "#8ED6FF"
                          fill()

                    return False

  place top $ t1 <> t2

{-
  slider <- liftIO $ newBehavior Nothing
  TheEventQueue eventQ <- theEventQueue <$> askSlideStyle

  place top $ tile (100,100) $ \ (x,y) (w,h) ->
    (listen $ do
      event <- readTChan eventQ
      if eType event == "mousemove"
      then setBehavior slider (Just event)
      else retry) <>
    (actOnBehavior slider $ \ v -> do
        liftIO $ print v
        return False)
-}
{-
  place top $ tile (500,50) $ \ (x,y) (w,h) -> do

    let bar = do
          beginPath()
          moveTo(25,25)
          lineTo(500 - 25,25)
          strokeStyle "blue"
          stroke()
          closePath()

    action $ do
        translate(x,y)
        clearRect(0,0,w,h)
        bar

    onEvent slider $ \ n -> do
          let n' = min n 1
          translate(x,y)
          clearRect(0,0,w,h)
          bar
          saveRestore $ do
            translate(x,y)
            clearRect(0,0,w,h)
            beginPath()
            rect(1,1,w-(n'*1000+2),h-2)
            strokeStyle "red"
            lineWidth 1
            stroke()
            closePath()
            return (n >= 1)
-}
  return ()
-}

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

subSuperSlide :: Slide ()
subSuperSlide = margin 20 $ fontSize 20 $ font "Gill Sans" $ do

  -- Try sub and super
  sequence_
      [ fontSize sz $ ul $ do
          li $ "The" <> sub "quick" <+> "brown" <> super ("fox") <+> "jumped" <+>
               "over" <+> "the" <> super ("lazy" <> super "dog") <> "." <+>
               "10" <> super "10" <+>
               "1"<>super "st" <+> "2"<>super "nd" <+> "3"<>super "rd"
      | sz <- [10, 12, 16, 20, 28, 48 ]
      ]


{-
tileAddress :: Size Double -> IO (Tile (),Behavior (Coord Double,Size Double))
tileAddress (w,h) = do
  var <- newTVarIO ((0,0),(0,0))

  let t = Tile (w,h) $ \ (x,y) (w',h') -> action $ liftIO $ atomically $ writeTVar var ((x,y),(w',h'))

  s <- atomically $ sample $ readTVar $ var

  return (t,s)
-}

rec :: Int -> Tile ()
rec 0 = box defaultBoxStyle $ blank (1,1)
rec n = do
  let t = nudge top left $ rec (n-1)
  if odd n then column [t,t] else row [t,t]

cachingTiles :: Slide ()
cachingTiles = margin 20 $ fontSize 20 $ font "Gill Sans" $ do
  t1 <- cacheTile ".cache/slow-tile.png" $ rec 12
  p $ "1"
  place top $ row [t1,t1,t1,t1,t1]
  p $ "2"
  place top $ row [t1,t1,t1,t1,t1]

scalingTiles :: Slide ()
scalingTiles = margin 20 $ fontSize 20 $ font "Gill Sans" $ do
  i1 <- scaledImageTile "example/images/KU.png" 0.1
  i2 <- scaledImageTile "example/images/KUlogo1C.png" 0.9
  i3 <- scaledImageTile "example/images/KU.png" 0.2
  p $ "1"
  place top i2
  p $ "2"
  place top $ row [i1,i2,i3]
