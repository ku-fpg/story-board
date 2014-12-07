{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs, StandaloneDeriving, TypeFamilies, DataKinds #-}
module Graphics.Storyboard
  ( -- * Markup
    p
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
  , space
  , br
  , boxy
    -- * Spacing
  , vspace
  , (<+>)
  , (</>)
  , align
  , getCavitySize
  , imageTile
  , scaledImageTile
  , draw
  , anchor
  , blank
  , filled
  , storyBoard
  , indent
  , margin
  , hr
  , Prose
  , prose
  , leftMargin
  , rightMargin
    -- * Tiles
  , Tile
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
--   , colorTile
  , gap
  , pack
--  , environment
--  , item
  , tileOfSlide
  , place
  , module Graphics.Storyboard.Box
   -- * Slide
   , cacheTile
   , Slide
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
  ) where

import           Data.Text (Text)
import           Data.Time.Clock

import           Graphics.Blank hiding (eval, font, port, Options)
import           Graphics.Storyboard.Act
import           Graphics.Storyboard.Behavior
-- import           Graphics.Storyboard.Bling
import           Graphics.Storyboard.Box
import           Graphics.Storyboard.Deck as Deck
import           Graphics.Storyboard.Highlight
import           Graphics.Storyboard.Images
-- import           Graphics.Storyboard.Layout
import           Graphics.Storyboard.Literals
import           Graphics.Storyboard.Markup
import           Graphics.Storyboard.Mosaic
import           Graphics.Storyboard.Paragraph
import qualified Graphics.Storyboard.Prelude as Prelude
import           Graphics.Storyboard.Prose
import           Graphics.Storyboard.Slide
import           Graphics.Storyboard.Tile
import           Graphics.Storyboard.Types

import           Text.Printf

-- blank margin around a story.
margin :: Double -> Slide a -> Slide a
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
  draw $ anchor top $ tile (w,2) $ \ (Cavity _ (w',_)) -> do
              beginPath()
              moveTo(0,1)
              lineTo(w',0)
              lineWidth 2
              strokeStyle "black"
              stroke()

vspace :: Double -> Slide ()
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
  _ <- getCurrentTime
  let StoryBoardState slides n context _ opt = st
  print ("slideShowr" :: Text,n)
  let cxt = (defaultSlideStyle (eventQueue context) (width context,height context))
          { theSlideNumber = n, theLastSlide = length slides }
  let st0 = defaultSlideState (fullSize cxt)
  send context $ clearCanvas
  (_,st1) <- Prelude.startPrelude (runSlide (slides !! (n-1)) cxt st0) context

  let deck = theDeck st1
--  print deck

  next <- runDeck context (pauseDeck deck)

  case snapShot opt of
    Nothing -> return ()
    Just s -> do
      url <- send context $ toDataURL()
      let fileName = s ++ "/" ++ printf "%04d.png" n
      writeDataURL fileName url

  case next of
    ForwardSlide | whichSlide st < length (theSlides st)
                -> slideShowr st { whichSlide = whichSlide st + 1 }
    BackSlide    | whichSlide st > 1
                -> slideShowr st { whichSlide = whichSlide st - 1 }
    _ -> slideShowr st
