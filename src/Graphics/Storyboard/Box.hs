{-# LANGUAGE OverloadedStrings #-}

module Graphics.Storyboard.Box where

import           Data.Monoid
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Text (Text)

import           Graphics.Blank as Blank
import qualified Graphics.Blank.Style as Style
import           Graphics.Storyboard.Act
import           Graphics.Storyboard.Literals
import           Graphics.Storyboard.Mosaic
import           Graphics.Storyboard.Tile
import           Graphics.Storyboard.Types

data TheBoxStyle = TheBoxStyle
  { theBorderWidth   :: Double
  , theBorderColor   :: Color
  , theBackground    :: Background
  , theShadowStyle   :: Maybe TheShadowStyle
  , theSharedBorders :: Set Side            -- ^ a shared border is straight, and perhaps offset
  } deriving Show

defaultBoxStyle :: TheBoxStyle
defaultBoxStyle = TheBoxStyle
  { theBorderWidth   = 1
  , theBorderColor   = "black"
  , theBackground    = bgColor "white"
  , theShadowStyle   = Just defaultShadowStyle
  , theSharedBorders = Set.empty
  }

data TheShadowStyle = TheShadowStyle
  { theShadowColor   :: Text
  , theShadowOffsetX :: Double
  , theShadowOffsetY :: Double
  , theShadowBlur    :: Double
  } deriving Show

defaultShadowStyle :: TheShadowStyle
defaultShadowStyle  = TheShadowStyle
  { theShadowColor   = "#cccccc"
  , theShadowOffsetX = 5
  , theShadowOffsetY = 5
  , theShadowBlur    = 5
  }

{-
-- Later support LinearGradients
data TheBackgroundStyle
  = Background Text
  | LinearGradient Text Text
  deriving Show
-}

class BoxStyle a where
  boxStyle :: (TheBoxStyle -> TheBoxStyle) -> a -> a

instance BoxStyle TheBoxStyle where
  boxStyle f a = f a

{-
class ShadowStyle a where
  shadowStyle :: (TheShadowStyle -> TheShadowStyle) -> a -> a

instance ShadowStyle TheShadowStyle where
  shadowStyle f a = f a

class BackgroundStyle a where
  backgroundStyle :: (TheBackgroundStyle -> TheBackgroundStyle) -> a -> a

instance BackgroundStyle TheBackgroundStyle where
  backgroundStyle f a = f a

instance BackgroundStyle TheBoxStyle where
  backgroundStyle f a = f a
-}

background :: BoxStyle a => Background -> a -> a
background bg  = boxStyle $ \ m -> m { theBackground = bg }


shadows :: BoxStyle a => Bool -> a -> a
shadows s = boxStyle $ \ m -> m
  { theShadowStyle =
        if s
        then Just defaultShadowStyle
        else Nothing
  }

borderWidth :: BoxStyle a => Double -> a -> a
borderWidth w = boxStyle $ \ m -> m { theBorderWidth = w }
borderColor :: BoxStyle a => Color -> a -> a
borderColor c = boxStyle $ \ m -> m { theBorderColor = c }

box :: TheBoxStyle -> Tile a -> Tile a
box st (Tile (w,h) act) = Tile (w+wd*2,h+wd*2) $ \ (Cavity ps' sz') ->
    action (before ps' sz') <>
    during ps' sz'          <>
    action (after ps' sz')

  where
    wd = theBorderWidth st

    before (x,y) (w',h') = saveRestore $ do
        translate (x,y)
        case theBackground st of
          Background bg -> Style.fillStyle bg
        -- Our backgrounds are scaled to ((0,0),(1,1))
        scale(w',h')
        beginPath()
        rect(0,0,1,1)
        closePath()
--        lineWidth wd
        case theShadowStyle st of
          Nothing -> return ()
          Just s_st -> do
            shadowColor (theShadowColor s_st)
            shadowOffsetX (theShadowOffsetX s_st)
            shadowOffsetY (theShadowOffsetY s_st)
            shadowBlur (theShadowBlur s_st)
        fill()
    during (x,y) (w',h') =
        act (Cavity (x+wd,y+wd) (w' - wd * 2,h' - wd * 2))
    after (x,y) (w',h')
      | wd == 0 = return ()
      | otherwise = saveRestore $ do
          translate (x,y)
          beginPath()
          rect(0,0,w',h')
          closePath()
          lineWidth wd
          strokeStyle (theBorderColor st)
          stroke()


-- Build a 2D table of boxes
boxes :: TheBoxStyle -> [[(TheBoxStyle -> TheBoxStyle,Tile ())]] -> Tile ()
boxes st tss = pack $ mconcat $
             [ anchor top $ pack $ mconcat
                [ anchor left $ box (f st) $ t
                | (f,t) <- ts
                ]
             | ts <- tss
             ]
