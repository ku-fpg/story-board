{-# LANGUAGE OverloadedStrings #-}

module Graphics.Storyboard.Box where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.Blank as Blank
import qualified Graphics.Blank.Style as Style

import Graphics.Storyboard.Literals
import Graphics.Storyboard.Slide
import Graphics.Storyboard.Tile


data TheBoxStyle = TheBoxStyle
  { theBorderWidth   :: Float
  , theBorderColor   :: Text
  , theBackground    :: Background
  , theShadowStyle   :: Maybe TheShadowStyle
  , theSharedBorders :: Set Side            -- ^ a shared border is straight, and perhaps offset
  }

defaultBoxStyle :: TheBoxStyle
defaultBoxStyle = TheBoxStyle
  { theBorderWidth   = 1
  , theBorderColor   = "black"
  , theBackground    = Background "white"
  , theShadowStyle   = Just defaultShadowStyle
  , theSharedBorders = Set.empty
  }

data TheShadowStyle = TheShadowStyle
  { theShadowColor   :: Text
  , theShadowOffsetX :: Float
  , theShadowOffsetY :: Float
  , theShadowBlur    :: Float
  }

defaultShadowStyle :: TheShadowStyle
defaultShadowStyle  = TheShadowStyle
  { theShadowColor   = "#cccccc"
  , theShadowOffsetX = 5
  , theShadowOffsetY = 5
  , theShadowBlur    = 5
  }

-- Later support LinearGradients
data Background
  = Background Text
  | LinearGradient Text Text

box :: TheBoxStyle -> Tile a -> Tile a
box st (Tile (w,h) act) = Tile (w+wd*2,h+wd*2) $ \ sz' -> do
    before sz'
    r <- during sz'
    after sz'
    return r

  where
    wd = theBorderWidth st
    Background bg = theBackground st

    before (w',h') = saveRestore $ do

        case theBackground st of
          Background bg -> Blank.fillStyle bg
          LinearGradient c0 c1 -> do
            grd <- createLinearGradient(0, 0, 0, h')
            grd # addColorStop(0, c0)
            -- dark blue
            grd # addColorStop(1, c1)
            Style.fillStyle grd

        beginPath()
        rect(0,0,w',h')
        closePath()
        lineWidth wd
        case theShadowStyle st of
          Nothing -> return ()
          Just s_st -> do
            shadowColor (theShadowColor s_st)
            shadowOffsetX (theShadowOffsetX s_st)
            shadowOffsetY (theShadowOffsetY s_st)
            shadowBlur (theShadowBlur s_st)
        fill()
    during (w',h') = saveRestore $ do
        translate (wd,wd)
        act (w' - wd * 2,h' - wd * 2)
    after (w',h') = saveRestore $ do
        beginPath()
        rect(0,0,w',h')
        closePath()
        lineWidth wd
        strokeStyle (theBorderColor st)
        stroke()
