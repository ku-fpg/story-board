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
import Control.Concurrent.STM

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

-- TODO: use Cavity, rather than Coord Double * Size Double

data Tile a = Tile (Size Double)
--                    (Coord Double -> Size Double -> Act)
                    (Cavity Double -> Act)

instance Show (Tile a) where
  show (Tile sz _) = show sz

--instance Functor Tile where
--  fmap f (Tile sz g) = Tile sz $ \ ps sz -> fmap f (g ps sz)

-- | tile requests a specific (minimum) size, and provides
-- a paint routine that takes the *actual* cavity to paint in.

tile :: Size Double -> (Cavity Double -> Canvas ()) -> Tile a
tile sz f = Tile sz $ \ cavity@(Cavity pos _) -> action $ saveRestore $ do
          translate pos
          f cavity

actile :: Size Double -> Behavior (Canvas Bool) -> Tile a
actile sz bhr = Tile sz $ \ cavity@(Cavity pos _) ->
               actOnBehavior $ \ env ->
               evalBehavior cavity env $ translateBehavior pos bhr

tileWidth :: Tile a -> Double
tileWidth (Tile (w,_) _) = w

tileHeight :: Tile a -> Double
tileHeight (Tile (_,h) _) = h

tileSize :: Tile a -> Size Double
tileSize (Tile sz _) = sz

blank :: Size Double -> Tile a
blank sz = tile sz $ const $ return ()

{-
colorTile :: Text -> Size Double -> Tile ()
colorTile col (w',h') = tile (w',h') $ \ (Cavity (x,y) (w,h)) -> do
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
-}


-- compress a tile into a point.
point :: Vertical -> Horizontal -> Tile a -> Tile a
point ver hor (Tile (w,h) f) = Tile (0,0) $ \ (Cavity (x,y) _) -> do
  let w' = case hor of
            HL -> 0
            HC -> -w / 2
            HR -> -w
      h' = case ver of
            VT -> 0
            VC -> -h / 2
            VB -> -h
  f (Cavity (x+w',y+h') (w,h))
-- nudge the tile into a specific corner of its enclosure
nudge :: Vertical -> Horizontal -> Tile a -> Tile a
nudge ver hor (Tile (w,h) f) = Tile (w,h) $ \ (Cavity (x,y) (w',h')) ->
    let w'' = case hor of
                HL -> 0
                HC -> (w' - w) / 2
                HR -> w' - w
        h'' = case ver of
                VT -> 0
                VC -> (h' - h) / 2
                VB -> h' - h
    in
         f (Cavity (x+w'',y+h'') (w,h))   -- and pretend there is no extra space

instance Semigroup (Tile a) where
  (Tile (x1,y1) c1) <> (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) (c1 <> c2)

instance Monoid (Tile a) where
  mempty = blank (0,0)
  (Tile (x1,y1) c1) `mappend` (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) (c1 `mappend` c2)

drawTile :: Drawing picture => Size Double -> picture -> Tile ()
drawTile (w',h') pic = tile (w',h') $ \ (Cavity (x,y) (w,h)) -> drawCanvas (w,h) pic

drawMovieTile :: (Playing movie, Drawing picture) => Size Double -> movie picture -> Tile ()
drawMovieTile (w',h') movie = case wrapMovie movie of
    Movie bhr f stop -> actile (w',h')
        ((\ (Cavity (x,y) (w,h)) b -> do
                    drawCanvas (w',h') $ f b
                    return (stop b)) <$> cavityB <*> bhr)
