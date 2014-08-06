{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Mosaic
  ( Mosaic
  , cavityMaxSize
  , anchor
  , pack
  , gap
  , vbrace
  , hbrace
  , fillTile
  ) where

import qualified Data.Text as Text
import Data.Text(Text)
import Data.List as List
import Control.Applicative
import Control.Monad (liftM2)
import Data.Semigroup
import Data.Text(Text)
import Graphics.Blank (Canvas,translate,saveRestore, console_log)
import Control.Monad.IO.Class
import Control.Lens (makeLenses)

import GHC.Exts (IsString(fromString))

import Graphics.Storyboard.Types.Basic
import Graphics.Storyboard.Literals
import Graphics.Storyboard.Environment
import Graphics.Storyboard.Tile

data Cavity f = Cavity
  { cavityCorner :: Coord f
  , cavitySize   :: Size f
  , cavitySpacer :: Size f  -- take height *or* width, not both
  }
  deriving Show

data Spacing'
  = Alloc Float    -- take up space
  | AtLeast Float  -- be at least this wide
  | Space'         -- space Mosaic
  deriving (Eq, Ord, Show)


-----------------------------------------------------------------------------

-- Anchored ??
-- Placed ??

data Mosaic a = Mosaic
  { mosaicSpace :: [(Spacing',Spacing')]
  , runMosaic   :: Cavity Float -> Canvas (a,Cavity Float)
  }

instance Functor Mosaic where
 fmap f m = pure f <*> m

instance Applicative Mosaic where
 pure a = Mosaic [] $ \ sz0 -> return (a,sz0)
 Mosaic fs f <*> Mosaic xs x = Mosaic (fs ++ xs) $ \ sz0 -> do
                    (f',sz1) <- f sz0
                    (x',sz2) <- x sz1
                    return (f' x',sz2)

instance Semigroup a => Semigroup (Mosaic a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Mosaic a) where
  mempty = pure mempty
  mappend = liftA2 mappend

cavityMaxSize :: Mosaic a -> Size Float -> Size Float
cavityMaxSize moz sz = (fst h,fst w)
    where (h,w) = cavityRange moz sz

cavityRange :: Mosaic a -> Size Float -> Size (Float,Float)
cavityRange (Mosaic sps _) (h,w) = ( foldl f (h,0) $ map fst sps
                                   , foldl f (w,0) $ map snd sps
                                   )
    where
          f (x,x') (Alloc n)   = (x - n,(x' - n) `max` 0)
          f (x,x') (AtLeast n) = (n `max` x,n `max` x')
          f (x,x') (Space')    = (x,0) -- assumes you want the max cavity??


-----------------------------------------------------------------------------

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
      let sw = if cw + w' < w || w_sps == 0 then 0 else (cw + w' - w) / w_sps
      let sh = if ch + h' < h || h_sps == 0 then 0 else (ch + h' - h) / h_sps
      console_log $ ("fillTile:" :: Text)
            <> show' cavity
            <> show' (w,h)
            <> show' (w',h')
            <> show' (w_sps,h_sps)
            <> show' (cw,ch)
            <> show' (sw,sh)
      fst <$> k (Cavity (0,0) (w',h') (sw,sh))
  where
    show' :: Show a => a -> Text
    show' = Text.pack . show

    w = foldr spaceSize 0 $ map fst $ cavity
    h = foldr spaceSize 0 $ map snd $ cavity

    (cwr,chr) = cavityRange mosaic (w,h)

    (cw,ch) = (fst cwr - snd cwr, fst chr - snd chr)


    w_sps = fromIntegral $ length [ () | Space' <- map fst cavity ]
    h_sps = fromIntegral $ length [ () | Space' <- map snd cavity ]

    spaceSize :: Spacing' -> Float -> Float
    spaceSize (Alloc n)   sz = sz + n
    spaceSize (AtLeast n) sz = sz `max` n
    spaceSize (Space')    sz = sz
