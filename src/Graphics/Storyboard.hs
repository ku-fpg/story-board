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
  , point
  , colorTile
  , gap
  , pack
--  , environment
--  , item
  , tileOfSlide
  , place
  , module Graphics.Storyboard.Box
   -- * adjusting sizes
  , trueSpace
   -- * timing
  , pause
  , theClock
   -- * colors
  , bgLinear
  , bgColor
  -- * Other
  , askSlideStyle
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
import Data.Time.Clock
import Control.Concurrent.STM

import Graphics.Storyboard.Act
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
  draw $ anchor top $ tile (w,2) $ \ (x,y) (w',h') -> action $ do
          saveRestore $ do
              translate (x,y)
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
blankCanvasStoryBoard slides context =
  slideShowr $ StoryBoardState
    { theSlides         = slides
    , whichSlide        = 1
    , theDeviceContext  = context
    , profiling         = True
    }
{-
  tm0 <- getCurrentTime
  send context $ do
    let cxt = defaultSlideStyle (width context,height context)
    let st0 = defaultSlideState (fullSize cxt)
    (_,st1) <- Prelude.startPrelude (runSlide (head slide) cxt st0) (eventQueue context)
    let Tile (w,h) m = fillTile (theMosaic st1)
    saveRestore $ do
      _ <- m (0,0) (w,h)
      return ()
    return ()
  tm1 <- getCurrentTime
  print $ diffUTCTime tm1 tm0
  return ()
-}

data StoryBoardState = StoryBoardState
  { theSlides         :: [Slide ()]
  , whichSlide        :: Int      -- starting at 1
  , theDeviceContext  :: DeviceContext
  , profiling         :: Bool     -- ^ do you output profiling information
  }

storyBoard :: [Slide ()] -> IO ()
storyBoard = blankCanvas 3000 { middleware = [], events = ["keypress"] }
           . blankCanvasStoryBoard

main :: IO ()
main = storyBoard [example3]

-- Never finishes
slideShowr :: StoryBoardState -> IO ()
slideShowr st = do
  tm0 <- getCurrentTime
  let StoryBoardState slides n context debug = st
  print ("slideShowr",n)
  clk <- newBehavior 0
  let cxt = defaultSlideStyle clk (width context,height context)
  let st0 = defaultSlideState (fullSize cxt)
  panels <- send context $ do
    clearCanvas
    (_,st1) <- Prelude.startPrelude (runSlide (slides !! (n-1)) cxt st0) (eventQueue context)
    sequence [ let Tile (w,h) m = pack moz
               in return $ m (0,0) (w,h)
             | moz <- theMosaic st1 : previousMosaics st1
             ]
  tm1 <- getCurrentTime
  when debug $ do
    putStrLn $ "profiling: Prelude for slide " ++ show n ++ " : " ++ show (diffUTCTime tm1 tm0)
  subSlideShowr st clk $ reverse panels

subSlideShowr :: StoryBoardState -> Behavior Float -> [Act] -> IO ()
subSlideShowr st _ [] = slideShowr st { whichSlide = whichSlide st + 1 }
subSlideShowr st clk (panel:panels) = do
  tm0 <- getCurrentTime
  print ("subSlideShowr",length (panel:panels))
  let StoryBoardState slides n context debug = st
{-
  let innerLoop n m [] = return (m,[])
      innerLoop n m (act:xs) = do

        innerLoop n (m >> a) xs

      innerLoop n m (r@(Replay end k):xs) = do
        print n
        -- Will always end with 'end'
        (m'',ys) <- innerLoop n (m >> k (min end n)) xs
        return (m'',if n >= end
                   then ys
                   else r : ys)
-}

  let outerLoop tm0 acts = do
        tm1 <- getCurrentTime
        let diff :: Float = realToFrac (diffUTCTime tm1 tm0)
        atomically $ setBehavior clk diff   -- set global timer
        done <- send context $ runAct acts
        if done
        then return ()
        else do d <- registerDelay (10 * 1000)
                let ev = do
                       event <- readTChan (eventQueue context)
                       if eType event == "keypress"
                         then return (Just event)
                         else retry
                let pz = do
                        b <- readTVar d
                        if b then return Nothing
                             else retry
                let lz = runListen acts >> return Nothing

                rz <- atomically $ ev `orElse` pz `orElse` lz
                case rz of
                  Just _ -> return ()
                  Nothing -> outerLoop tm0 acts

  start_tm <- getCurrentTime
  atomically $ setBehavior clk 0
  send context $ do
            runFirstAct panel
            runAct panel
  outerLoop start_tm panel
  tm1 <- getCurrentTime
  --  print "waiting for key"
  when debug $ do
    putStrLn $ "profiling: Frame for slide " ++ show n ++ " : " ++ show (diffUTCTime tm1 tm0)
  do
        event <- atomically $ do
          event <- readTChan (eventQueue context)
          if eType event == "keypress"
          then return event
          else retry
        print ("got key",event)
        case eWhich event of
          Just 98 -> slideShowr st { whichSlide = whichSlide st - 1 }
          _ -> subSlideShowr st clk panels
