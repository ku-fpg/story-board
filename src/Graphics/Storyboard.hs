{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs, StandaloneDeriving, TypeFamilies, DataKinds #-}
module Graphics.Storyboard where

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

spaceSize :: Spacing' -> Float -> Float
spaceSize (Alloc n)   sz = sz + n
spaceSize (AtLeast n) sz = sz `max` n
spaceSize (Space')     sz = sz

-----------------------------------------------------------------------------

-- | tile requests a specific (minimum) size, and provides
-- a paint routine that takes the *actual* size.
-- The paint routine can assume the canvas starts at (0,0),
-- and is the given size. No masking is done by default.

tile :: (Float,Float) -> ((Float,Float) -> Canvas a) -> Tile a
tile = Tile

center :: Tile a -> Filler a
center t = vfill *> (left gap *> left t <* hfill) <* vfill

data Side = T | B | L | R

-- Anchor?
class Filling f where
    anchor :: Side -> f a -> Filler a

left   :: Filling f => f a -> Filler a
left   = anchor L
right  :: Filling f => f a -> Filler a
right  = anchor R
top    :: Filling f => f a -> Filler a
top    = anchor T
bottom :: Filling f => f a -> Filler a
bottom = anchor B

instance Filling Tile where
  anchor side (Tile (w,h) k) = Filler [newSpacing side (w,h)] $
     \ cavity -> do
          a <- saveRestore $ do
              translate $ newOffset side (w,h) cavity
              k $ realTileSize side (w,h) cavity
          return (a,newCavity side (w,h) cavity)
instance Filling Gap where
  anchor L Gap = hfill

{-
instance Filling Parcel where
  left (Parcel n) = Filler [] $ \ (Cavity (cx,cy) (cw,ch) (sw,sh)) ->
    return ((),Cavity (cx,cy + sh) (cw,ch - sh) (sw,sh))

data Parcel :: * -> * where
  Parcel :: Float -> Parcel ()
-}
{-
    Filler [(Alloc 0,Space')] return <>
-}

--instance Filling Filler where
--  left f = f <* right gap

data Gap :: * -> * where
  Gap :: Gap ()

gap :: Gap ()
gap = Gap

column :: [Tile ()] -> Tile ()
column xs = fillTile $ mconcat $ intersperse vfill $ map tileLeft $ xs
row    :: [Tile a] -> Tile a
row = undefined

-----------------------------------------------------------------------------

vfillSpacing = [(Space', Alloc 0)]
hfillSpacing = [(Alloc 0, Space')]

-----------------------------------------------------------------------------

newSpacing :: Side -> Size Float -> (Spacing',Spacing')
newSpacing T (w,h) = (AtLeast w, Alloc h)
newSpacing B (w,h) = (AtLeast w, Alloc h)
newSpacing L (w,h) = (Alloc w, AtLeast h)
newSpacing R (w,h) = (Alloc w, AtLeast h)

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

--tileSide :: Side -> Cavity Float ->

-----------------------------------------------------------------------------


vfill :: Filler ()
vfill = Filler [(Space',Alloc 0)] $
  \ (Cavity (cx',cy') (cw',ch') spaces@(sw,sh)) -> do
      return ((),Cavity (cx' + sw,cy') (cw' - sw,ch') spaces)


hfill :: Filler ()
hfill = Filler [(Alloc 0,Space')] $
  \ (Cavity (cx',cy') (cw',ch') spaces@(sw,sh)) -> do
      return ((),Cavity (cx',cy' + sh) (cw',ch' - sh) spaces)


tileTop :: Tile a -> Filler a
tileTop (Tile (w,h) k) = Filler [(AtLeast w,Alloc h)] $
    \ (Cavity (cx',cy') (cw',ch') spaces) -> do
        a <- saveRestore $ do
                translate (cx',cy')
                k (cw',h)
        return (a,Cavity (cx',cy' + h) (cw',ch' - h) spaces)

tileLeft :: Tile a -> Filler a
tileLeft (Tile (w,h) k) = Filler [(Alloc w, AtLeast h)] $
    \ (Cavity (cx',cy') (cw',ch') spaces) -> do
        a <- saveRestore $ do
                translate (cx',cy')
                k (w,ch')
        return (a,Cavity (cx' + w,cy') (cw' - w,ch') spaces)

pack = fillTile
fillTile :: Filler a -> Tile a
fillTile (Filler cavity k) = Tile (w,h) $ \ (w',h') -> do
      let sw = if w' <= w then 0 else (w' - w) / w_sps
      let sh = if h' <= h then 0 else (h' - h) / h_sps
      fst <$> k (Cavity (0,0) (w',h') (sw,sh))
  where
    w = foldr spaceSize 0 $ map fst $ cavity
    h = foldr spaceSize 0 $ map snd $ cavity
    w_sps = fromIntegral $ length [ () | Space' <- map fst cavity ]
    h_sps = fromIntegral $ length [ () | Space' <- map snd cavity ]

-----------------------------------------------------------------------------

example1 :: Text -> Tile ()
example1 col = Tile (100,100) $ \ sz@(w,h) -> do

        -- background
        beginPath()
        strokeStyle "#dddddd"
        rect(0,0,w,h)
        closePath()
        stroke()

        -- assumes 100 x 100 pixels sized viewport
        beginPath()
        fillStyle col
        arc(50, 50, 40, 0, pi*2, False)
        closePath()
        fill()


example2 :: Filler ()
example2 =
  hfill *>
  (top $ example1 "red")    *>
  (top $ column [ example1 "red" | i <- [0..7]]) *>
  (top $ example1 "green")  *>
  (left $ example1 "pink")    *> vfill *>
  (left$ example1 "blue")  *>
  (top $ example1 "orange")  *>
  (left $ example1 "pink")    *>
  (left$ example1 "blue")  *> vfill *>
  (left $ example1 "pink")    *>
  (left$ example1 "blue")  *>
  (right$ example1 "#123456")  *>
  pure ()

main = do
    print cavity
    print $ foldr spaceSize 0 $ map fst $ cavity
    print $ foldr spaceSize 0 $ map snd $ cavity
--    putStrLn $ unlines $ map prettyEq $ nub eqs
  where
    Filler cavity _ = example2

--          Tile _ m = fillTile (width context - 200,height context - 200) $ example2

main2 = blankCanvas 3000 $ \ context -> do
      send context $ do
        fillStyle "orange"
        translate (100,100)
        let Tile (w,h) m = fillTile example2
        _ <- m (w + 100,h + 100)
        return ()
