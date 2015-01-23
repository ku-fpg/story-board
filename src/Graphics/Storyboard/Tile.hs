{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Tile where

import Control.Applicative
import Data.Text (Text)

import Data.Semigroup

import Graphics.Blank
import Graphics.Storyboard.Act
import Graphics.Storyboard.Behavior
import Graphics.Storyboard.Literals
import Graphics.Storyboard.Types

-- | A Tile has a specific, fixed size.
-- When rendered, it is given a specific size to operate inside of,
-- that typically would be *at least* the size of the original fixed size.
-- The tile can choose to put any extra space on the inside or outside
-- of any border, etc.

data Tile a = Tile (Size Double) (Cavity Double -> Act)

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

-- Used mainly for debugging blank tiles.
filled :: Text -> Size Double -> Tile ()
filled col (w',h') = tile (w',h') $ \ (Cavity (x,y) (w,h)) -> saveRestore $ do
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

-- Compress a tile into a point.
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

-- Nudge the tile into a specific corner of its enclosure.
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
drawTile (w',h') pic = tile (w',h') $ \ (Cavity _ (w,h)) -> drawCanvas (w,h) pic

drawMovieTile :: (Playing movie, Drawing picture) => Size Double -> movie picture -> Tile ()
drawMovieTile (w',h') movie = case wrapMovie movie of
    Movie bhr f stop -> actile (w',h')
        ((\ (Cavity _ _) b -> do
                    drawCanvas (w',h') $ f b
                    return (stop b)) <$> cavityB <*> bhr)


--scaleTile :: Double -> Tile () -> Tile ()
--scaleTile n (Tile (w,h) act) = Tile (w * n,h * n) $ \ (Cavity pos loc) ->
