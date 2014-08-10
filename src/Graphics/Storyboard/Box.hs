{-# LANGUAGE OverloadedStrings #-}

module Graphics.Storyboard.Box where

import Data.Text (Text)
import Graphics.Blank

import Graphics.Storyboard.Slide
import Graphics.Storyboard.Tile


data TheBoxStyle = TheBoxStyle
  { theBorderWidth   :: Float
  , theBorderColor   :: Text
  , theBackground    :: Background
  , theShadowStyle   :: Maybe TheShadowStyle
  }

defaultBoxStyle :: TheBoxStyle
defaultBoxStyle = TheBoxStyle
  { theBorderWidth   = 1
  , theBorderColor   = "black"
  , theBackground    = Background "white"
  , theShadowStyle   = Just defaultShadowStyle
  }

data TheShadowStyle = TheShadowStyle
  { theShadowColor   :: Text
  , theShadowOffsetX :: Float
  , theShadowOffsetY :: Float
  , theShadowBlur    :: Float
  }

defaultShadowStyle :: TheShadowStyle
defaultShadowStyle  = TheShadowStyle
  { theShadowColor   = "black"
  , theShadowOffsetX = 5
  , theShadowOffsetY = 5
  , theShadowBlur    = 5
  }

-- Later support LinearGradients
newtype Background = Background Text

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
{-
        grd <- createLinearGradient(0, 0, 0, h')
        -- light blue
        grd # addColorStop(0, "#8EA154")
        -- dark blue
        grd # addColorStop(1, "#7D8E4B")
        Style.fillStyle grd;
-}
        fillStyle bg
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
