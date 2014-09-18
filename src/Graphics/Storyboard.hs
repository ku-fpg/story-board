{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs, StandaloneDeriving, TypeFamilies, DataKinds #-}
module Graphics.Storyboard
  ( Slide
    -- * Markup
  , p
  , ul
  , ol
  , li
    -- * Prose
  , ProseStyle
  , i
  , b
  , font
  , fontSize
  , scaleFont
  , big
  , small
  , color
  , plain
  , wordSpacing
  , ligature
  , noLigatures
  , super
  , sub
  , boxy
    -- * Spacing
  , vspace
  , (<+>)
  , (</>)
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
  , leftMargin
  , rightMargin
    -- * Tiles
  , tile
  , tileSize
  , tileWidth
  , tileHeight
  , nudge
  , column
  , row
    -- Tables
  , tr, td, table
  , background
  , frame
    -- * Mosaic
  , hbrace
  , vbrace
    -- * Types
  , Background
  , Size
  , Coord
  , MonadCanvas
    -- * Highlights
  , highlight
  , defaultHighlightStyle
  , haskellHighlightStyle
  , ghciHighlightStyle
  , TheHighlightStyle
    -- * Useful literals
  , module Graphics.Storyboard.Literals
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
--  , theClock
   -- * Colors
  , Color
  , bgLinear
  , bgColor
  -- * Internal State and Environment
  , TheSlideStyle(..)
  -- * Other
  , askSlideStyle
  , actOnBehavior
  -- * Plugin to other
  , Drawing
  , Playing
  , drawTile
  , drawMovieTile
  , Movie -- not sure about this; do we need to export this?
  , Options(..)
  )


where

import Graphics.Blank hiding (eval,font, port, Options)
import qualified Graphics.Blank as Blank
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Data.List
import Data.Maybe
import Text.Printf
import Control.Monad.IO.Class
import Data.Time.Clock
import Control.Concurrent.STM

import Graphics.Storyboard.Act
import Graphics.Storyboard.Behavior
import Graphics.Storyboard.Deck      as Deck
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

blankCanvasStoryBoard :: Options -> [Slide ()] -> DeviceContext -> IO ()
blankCanvasStoryBoard opts slides context =
  slideShowr $ StoryBoardState
    { theSlides         = slides
    , whichSlide        = 1
    , theDeviceContext  = context
    , profiling         = True
    , options           = opts
    }

data StoryBoardState = StoryBoardState
  { theSlides         :: [Slide ()]
  , whichSlide        :: Int      -- starting at 1
  , theDeviceContext  :: DeviceContext
  , profiling         :: Bool     -- ^ do you output profiling information
  , options           :: Options
  }

data Options = Options
  { snapShot          :: Maybe String
  , port              :: Int
  }

instance Num Options where
  fromInteger n = Options { snapShot = Nothing, port = fromIntegral n }

storyBoard :: Options -> [Slide ()] -> IO ()
storyBoard opt
        = blankCanvas (fromIntegral (port opt)) { middleware = [], events = ["keypress","mousemove"] }
        . blankCanvasStoryBoard opt

-- Never finishes
slideShowr :: StoryBoardState -> IO ()
slideShowr st = do
  tm0 <- getCurrentTime
  let StoryBoardState slides n context debug opt = st
  print ("slideShowr",n)
  let cxt = (defaultSlideStyle (eventQueue context) (width context,height context))
          { theSlideNumber = n, theLastSlide = length slides }
  let st0 = defaultSlideState (fullSize cxt)
  send context $ clearCanvas
  (_,st1) <- Prelude.startPrelude (runSlide (slides !! (n-1)) cxt st0) context

  let deck = theDeck st1
  print deck

  _ <- runDeck context (pauseDeck deck)

  case snapShot opt of
    Nothing -> return ()
    Just s -> do
      url <- send context $ toDataURL()
      let fileName = s ++ "/" ++ printf "%04d.png" n
      writeDataURL fileName url

  print "Done"
  return ()

  slideShowr st { whichSlide = whichSlide st + 1 }

{-

  let panels =
         [ let Tile (w,h) m = pack moz
           in m (0,0) (fullSize cxt)--(w,h)
         | moz <- theMosaic st1 : previousMosaics st1
         ]
  tm1 <- getCurrentTime
  when debug $ do
    putStrLn $ "profiling: Prelude for slide " ++ show n ++ " : " ++ show (diffUTCTime tm1 tm0)
  subSlideShowr st $ reverse panels
-}
subSlideShowr :: StoryBoardState -> [Act] -> IO ()
subSlideShowr st [] = slideShowr st { whichSlide = whichSlide st + 1 }
subSlideShowr st (panel:panels) = do
  tm0 <- getCurrentTime
  print ("subSlideShowr",length (panel:panels))
  let StoryBoardState slides n context debug opt = st
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

  start_tm <- getCurrentTime
  -- Draw the basic static stuff
  send context $ runFirstAct panel
  let outerLoop behEnv0 acts = do
        -- First, animate the frame
        theAct <- runAct behEnv0 acts
        done <- send context theAct
        if done
        then return ()
        else do -- next figure out the events
                d <- registerDelay (10 * 1000)
                let ev = do
                       event <- readTChan (eventQueue context)
                       return (Just event)
                let pz = do
                        b <- readTVar d
                        if b then return Nothing
                             else retry

                rz <- atomically $ ev `orElse` pz

                -- next, reset the env
                tm1 <- getCurrentTime
                let diff :: Float = realToFrac (diffUTCTime tm1 start_tm)
                let behEnv1 = nextBehaviorEnv diff rz behEnv0

                case rz of
                  Just event | eType event == "keypress" ->
                      do atomically $ unGetTChan (eventQueue context) event
                         return ()
                  _ -> outerLoop behEnv1 acts

  -- And animate!
  outerLoop defaultBehaviorEnv panel
  tm1 <- getCurrentTime
  --  print "waiting for key"
  when debug $ do
    putStrLn $ "profiling: Frame for slide " ++ show n ++ " : " ++ show (diffUTCTime tm1 tm0)
  do
        case snapShot opt of
          Nothing -> return ()
          Just s -> do
            url <- send context $ toDataURL()
            let fileName = s ++ "/" ++ printf "%04d.png" n
            writeDataURL fileName url

        let loop = do
                  event <- readTChan (eventQueue context)
                  if eType event == "keypress"
                  then return event
                  else loop -- ignore other things, for now
        event <- atomically $ loop
        print ("got key",event)
        case eWhich event of
          Just 98 -> slideShowr st { whichSlide = whichSlide st - 1 }
          _ -> subSlideShowr st panels
