{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Tile where

import qualified Data.Text as Text
import Data.Text(Text)
import Data.List as List
import Control.Applicative
import Control.Monad (liftM2)
import Data.Semigroup
import Data.Text(Text)
import Graphics.Blank (Canvas)
import Control.Monad.IO.Class

import GHC.Exts (IsString(fromString))

import Graphics.Storyboard.Types
import Graphics.Storyboard.Literals

import Graphics.Blank

-- | A Tile has a specific, fixed size.
-- When rendered, it is given a specific size to operate inside of,
-- that typically would be *at least* the size of the original fixed size.
-- The tile can choose to put any extra space on the inside or outside
-- of any border, etc.

data Tile a = Tile (Size Float) (Coord Float -> Size Float -> Canvas a)

instance Show (Tile a) where
  show (Tile sz _) = show sz


instance Functor Tile where
  fmap f (Tile sz g) = Tile sz $ \ ps sz -> fmap f (g ps sz)

-- | tile requests a specific (minimum) size, and provides
-- a paint routine that takes the *actual* size.
-- The paint routine can assume the canvas starts at (0,0),
-- and is the given size. No masking is done by default.

tile :: Size Float -> (Coord Float -> Size Float -> Canvas a) -> Tile a
tile = Tile

tileWidth :: Tile a -> Float
tileWidth (Tile (w,_) _) = w

tileHeight :: Tile a -> Float
tileHeight (Tile (_,h) _) = h

tileSize :: Tile a -> Size Float
tileSize (Tile sz _) = sz

blank :: Size Float -> Tile ()
blank sz = tile sz $ const $ const $ return ()

colorTile :: Text -> Size Float -> Tile ()
colorTile col (w',h') = tile (w',h') $ \ (x,y) (w,h) -> do
    globalAlpha 0.2
    beginPath()
    rect(0, 0, w, h)
    fillStyle col
    fill();
    globalAlpha 0.5
    beginPath()
    rect(0, 0, w, h);
    lineWidth 1;
    strokeStyle "black";
    stroke()
    globalAlpha 1.0

-- compress a tile into a point.
point :: Vertical -> Horizontal -> Tile a -> Tile a
point ver hor (Tile (w,h) f) = Tile (0,0) $ \ (x,y) _ -> do
  let w' = case hor of
            HL -> 0
            HC -> -w / 2
            HR -> -w
      h' = case ver of
            VT -> 0
            VC -> -h / 2
            VB -> -h
  saveRestore $ do
    translate (w',h')
    f (x+w',y-h') (w,h)

-- nudge the tile into a specific corner of its enclosure
nudge :: Vertical -> Horizontal -> Tile a -> Tile a
nudge ver hor (Tile (w,h) f) = Tile (w,h) $ \ (x,y) (w',h') ->
    let w'' = case hor of
                HL -> 0
                HC -> (w' - w) / 2
                HR -> w' - w
        h'' = case ver of
                VT -> 0
                VC -> (h' - h) / 2
                VB -> h' - h
    in
       saveRestore $ do
         translate (w'',h'')     -- nudge
         f (x-w'',y-h'') (w,h)   -- and pretend there is no extra space



instance Semigroup a => Semigroup (Tile a) where
  (Tile (x1,y1) c1) <> (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $ \ ps sz ->
        do r1 <- c1 ps sz
           r2 <- c2 ps sz -- overlay is the default monoid
           return (r1 <> r2)

instance Monoid a => Monoid (Tile a) where
  mempty = Tile (0,0) (return mempty)
  (Tile (x1,y1) c1) `mappend` (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $ \ ps sz ->
      do r1 <- c1 ps sz
         r2 <- c2 ps sz -- overlay is the default monoid
         return (r1 `mappend` r2)
