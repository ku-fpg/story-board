{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs, StandaloneDeriving, TypeFamilies, DataKinds #-}

module Graphics.Storyboard.Layout where

import Graphics.Blank hiding (eval)
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Data.List
import Data.Maybe

import Graphics.Storyboard.Types

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

--center :: Tile a -> Mosaic a
--center t = anchor top gap *> (anchor left gap *> anchor left t <* right gap) <* bottom gap

--data Side = T | B | L | R

-- Anchor?
class Filling f where
    anchor :: Side -> f a -> Mosaic a
{-
left   :: Filling f => f a -> Mosaic a
left   = anchor L
right  :: Filling f => f a -> Mosaic a
right  = anchor R
top    :: Filling f => f a -> Mosaic a
top    = anchor T
bottom :: Filling f => f a -> Mosaic a
bottom = anchor B
-}

instance Filling Tile where
  anchor side (Tile (w,h) k) = Mosaic [newSpacing side (w,h)] $
     \ cavity -> do
          a <- saveRestore $ do
              translate $ newOffset side (w,h) cavity
              k' $ realTileSize side (w,h) cavity
          return (a,newCavity side (w,h) cavity)
     where k' (w,h) = do
{-
              strokeStyle "red"
              lineWidth 1
              rect(0,0,w,h)
              closePath()
              stroke()
-}
              k (w,h)



instance Filling Gap where
  anchor side Gap = Mosaic [fillSpacing side] $
    \ cavity -> return ((),newSpacingCavity side cavity)


--  anchor T Gap = hfill

{-
instance Filling Parcel where
  left (Parcel n) = Mosaic [] $ \ (Cavity (cx,cy) (cw,ch) (sw,sh)) ->
    return ((),Cavity (cx,cy + sh) (cw,ch - sh) (sw,sh))

data Parcel :: * -> * where
  Parcel :: Float -> Parcel ()
-}
{-
    Mosaic [(Alloc 0,Space')] return <>
-}

--instance Filling Mosaic where
--  left f = f <* right gap

data Gap :: * -> * where
  Gap :: Gap ()

gap :: Gap ()
gap = Gap

blankTile :: Size Float -> Tile ()
blankTile sz = tile sz $ const $ return ()

-- brace that force the inside to be *at least* this size.
-- (Think Star Wars IV.)
vbrace :: Float -> Mosaic ()
vbrace h = anchor left (tile (0,h) $ const $ return ())

hbrace :: Float -> Mosaic ()
hbrace w = anchor top (tile (w,0) $ const $ return ())

column :: [Tile ()] -> Tile ()
column = fillTile . mconcat . intersperse (anchor left gap) . map (anchor left)

row :: [Tile ()] -> Tile ()
row = fillTile . mconcat . intersperse (anchor top gap) . map (anchor top)

-----------------------------------------------------------------------------

newSpacing :: Side -> Size Float -> (Spacing',Spacing')
newSpacing T (w,h) = (AtLeast w, Alloc h)
newSpacing B (w,h) = (AtLeast w, Alloc h)
newSpacing L (w,h) = (Alloc w, AtLeast h)
newSpacing R (w,h) = (Alloc w, AtLeast h)

-- Note that newCavity ignores either the width or height, as appropreate
newCavity :: Side -> Size Float -> Cavity Float -> Cavity Float
newCavity side (w,h) cavity = case side of
    T -> Cavity (cx,cy + h) (cw,ch - h) (sw,sh)
    B -> Cavity (cx,cy)     (cw,ch - h) (sw,sh)
    L -> Cavity (cx + w,cy) (cw - w,ch) (sw,sh)
    R -> Cavity (cx,cy)     (cw - w,ch) (sw,sh)
  where Cavity (cx,cy) (cw,ch) (sw,sh) = cavity

newOffset :: Side -> Size Float -> Cavity Float -> Coord Float
newOffset side (w,h) cavity = case side of
    T -> (cx,cy)
    B -> (cx,cy + ch - h)
    L -> (cx,cy)
    R -> (cx + cw - w,cy)
  where Cavity (cx,cy) (cw,ch) (sw,sh) = cavity

realTileSize :: Side -> Size Float -> Cavity Float -> Size Float
realTileSize side (w,h) cavity = case side of
    T -> (cw,h)
    B -> (cw,h)
    L -> (w,ch)
    R -> (w,ch)
  where Cavity (cx,cy) (cw,ch) (sw,sh) = cavity

fillSpacing :: Side -> (Spacing',Spacing')
fillSpacing side = case side of
    T -> (Alloc 0,Space')
    B -> (Alloc 0,Space')
    L -> (Space',Alloc 0)
    R -> (Space',Alloc 0)

newSpacingCavity :: Side -> Cavity Float -> Cavity Float
newSpacingCavity side cavity = newCavity side (sw,sh) cavity
  where Cavity (cx,cy) (cw,ch) (sw,sh) = cavity

-----------------------------------------------------------------------------

pack = fillTile

fillTile :: Mosaic a -> Tile a
fillTile mosaic@(Mosaic cavity k) = Tile (w,h) $ \ (w',h') -> do
      let sw = if w' <= w then 0 else (cw + w' - w) / w_sps
      let sh = if h' <= h then 0 else (ch + h' - h) / h_sps
      fst <$> k (Cavity (0,0) (w',h') (sw,sh))
  where
    w = foldr spaceSize 0 $ map fst $ cavity
    h = foldr spaceSize 0 $ map snd $ cavity

    (cw,ch) = cavitySize' mosaic (w,h)

    w_sps = fromIntegral $ length [ () | Space' <- map fst cavity ]
    h_sps = fromIntegral $ length [ () | Space' <- map snd cavity ]

    spaceSize :: Spacing' -> Float -> Float
    spaceSize (Alloc n)   sz = sz + n
    spaceSize (AtLeast n) sz = sz `max` n
    spaceSize (Space')    sz = sz
