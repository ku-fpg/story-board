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
import Graphics.Storyboard.Act
import Graphics.Storyboard.Behavior

import Graphics.Blank

-- | A Tile has a specific, fixed size.
-- When rendered, it is given a specific size to operate inside of,
-- that typically would be *at least* the size of the original fixed size.
-- The tile can choose to put any extra space on the inside or outside
-- of any border, etc.

-- TODO: use Cavity, rather than Coord Float * Size Float

data Tile a = Tile (Size Float) (Coord Float -> Size Float -> Act)

instance Show (Tile a) where
  show (Tile sz _) = show sz

--instance Functor Tile where
--  fmap f (Tile sz g) = Tile sz $ \ ps sz -> fmap f (g ps sz)

-- | tile requests a specific (minimum) size, and provides
-- a paint routine that takes the *actual* cavity to paint in.

tile :: Size Float -> (Coord Float -> Size Float -> Act) -> Tile a
tile = Tile

tileWidth :: Tile a -> Float
tileWidth (Tile (w,_) _) = w

tileHeight :: Tile a -> Float
tileHeight (Tile (_,h) _) = h

tileSize :: Tile a -> Size Float
tileSize (Tile sz _) = sz

blank :: Size Float -> Tile ()
blank sz = tile sz $ const $ const $ mempty

colorTile :: Text -> Size Float -> Tile ()
colorTile col (w',h') = tile (w',h') $ \ (x,y) (w,h) -> do
  action $ saveRestore $ do
    translate (x,y)   -- required in all primitives
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
  f (x+w',y+h') (w,h)
{-
  saveRestore $ do
    translate (w',h')
-}

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
--       saveRestore $ do
--         translate (w'',h'')     -- nudge
         f (x+w'',y+h'') (w,h)   -- and pretend there is no extra space


instance Semigroup a => Semigroup (Tile a) where
  (Tile (x1,y1) c1) <> (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $ \ ps sz ->
        c1 ps sz <> c2 ps sz

instance Monoid a => Monoid (Tile a) where
  mempty = Tile (0,0) (\ _ _ -> mempty)
  (Tile (x1,y1) c1) `mappend` (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $ \ ps sz ->
      c1 ps sz `mappend` c2 ps sz

drawTile :: Drawing picture => Size Float -> picture -> Tile ()
drawTile (w',h') pic = tile (w',h') $ \ (x,y) (w,h) -> action $ saveRestore $ do
      translate (x,y)   -- required in all primitives
      drawCanvas (w',h') pic

-- It might be possible to combine these two functions
drawMovieTile :: (Playing movie, Drawing picture) => Size Float -> movie picture -> Tile ()
drawMovieTile (w',h') movie = case wrapMovie movie of
    Movie bhr f stop -> tile (w',h') $ \ (x,y) (w,h) ->
     actOnBehavior bhr $ \ b ->
      saveRestore $ do
          translate (x,y)
          clearRect(0,0,w',h')
          drawCanvas (w',h') $ f b
          return (stop b)

--mapTileAct :: (Act a -> Act b) -> Tile a -> Tile b
--mapTileAct f (Tile (w,h) g) = Tile (w,h) $ \ ps sz -> f (g ps sz)
