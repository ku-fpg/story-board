{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Mosaic
  ( Mosaic
  , cavityMaxSize
  , cavityRange
  , (?)
  , anchor
  , pack
  , gap
  , vbrace
  , hbrace
  , blankMosaic
  , cavityOfMosaic
  , spacingInMosaic
  , row
  , column
  , Cavity
  , cavityCorner
  , cavitySize
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

import GHC.Exts (IsString(fromString))

import Graphics.Storyboard.Act
import Graphics.Storyboard.Types
import Graphics.Storyboard.Literals
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
  , runMosaic   :: Coord Float -> Cavity Float -> (Act,Cavity Float)
  }

instance Show (Mosaic a) where
  show m = "Mosaic" ++ show (mosaicSpace m)
{-
instance Functor Mosaic where
 fmap f m = pure f <*> m

instance Applicative Mosaic where
 pure a = Mosaic [] $ \ _ sz0 -> (return a,sz0)
 Mosaic fs f <*> Mosaic xs x = Mosaic (fs ++ xs) $ \ ps sz0 ->
                 let (f',sz1) = f ps sz0
                     (x',sz2) = x ps sz1
                 in (f' <*> x', sz2)

-}
instance Semigroup (Mosaic a) where
  Mosaic fs f <> Mosaic xs x = Mosaic (fs ++ xs) $ \ ps sz0 ->
                let (f',sz1) = f ps sz0
                    (x',sz2) = x ps sz1
                in (f' <> x', sz2)

instance Monoid (Mosaic a) where
  mempty = Mosaic [] $ \ _ sz0 -> (mempty,sz0)
  mappend = (<>)

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


-- Make a simple blank frame from a mosaic

-----------------------------------------------------------------------------

-- Take an outside size, and an boundary cavity,
-- and build a mosaic that have the size and shape
-- of the gap between the outside size, and inner cavity.

blankMosaic :: Size Float -> Cavity Float -> Mosaic ()
blankMosaic (w,h) cavity =
    anchor top    (blank (0,cy)) <>
    anchor bottom (blank (0,cy')) <>
    anchor left   (blank (cx,0)) <>
    anchor right  (blank (cy',0)) <>
    hbrace cw <>
    vbrace ch
  where Cavity (cx,cy) (cw,ch) (sw,sh) = cavity
        (cx',cy') = (w - (cx + cw), h - (cy + ch))


-----------------------------------------------------------------------------

infix 8 ?
(?) ::Tile a -> Side -> Mosaic a
(?) = flip anchor

anchor :: Side -> Tile a -> Mosaic a
anchor side (Tile (w,h) k) = Mosaic [newSpacing side (w,h)] $
     \ (x,y) cavity ->
            ( do let (x',y') = newOffset side (w,h) cavity
                 k (x+x',y+y') -- TODO: this is boxed in
                   (realTileSize side (w,h) cavity)
            , newCavity side (w,h) cavity
            )
{-

     where k' (w,h) = do
              strokeStyle "red"
              lineWidth 1
              rect(0,0,w,h)
              closePath()
              stroke()
              k (w,h)
-}


gap :: Side -> Mosaic ()
gap side = Mosaic [fillSpacing side] $
    \ ps cavity -> (mempty,newSpacingCavity side cavity)

-- brace that force the inside to be *at least* this size.
-- (Think Star Wars IV.)
vbrace :: Float -> Mosaic ()
vbrace h = anchor left (blank (0,h))

hbrace :: Float -> Mosaic ()
hbrace w = anchor top (blank (w,0))

column :: [Tile ()] -> Tile ()
column = pack . mconcat . intersperse (gap left) . map (anchor left)

row :: [Tile ()] -> Tile ()
row = pack . mconcat . intersperse (gap top) . map (anchor top)

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

{-
pack :: Mosaic a -> Tile a
pack = fmap fst . fillTile

cavityOfMosaic :: Mosaic a -> (Cavity Float)
-}

-- Hmm. this should assume zero spacing?
cavityOfMosaic :: Mosaic a -> Size Float -> Cavity Float
cavityOfMosaic mosaic@(Mosaic cavity k) (w',h')
  = snd $ k (0,0) $ Cavity (0,0) (w',h') $ spacingInMosaic mosaic (w',h')

spacingInMosaic :: Mosaic a -> Size Float -> Size Float
spacingInMosaic mosaic@(Mosaic cavity k) (w',h') =  (sw,sh)
  where
    sw = if cw + w' < w || w_sps == 0 then 0 else (cw + w' - w) / w_sps
    sh = if ch + h' < h || h_sps == 0 then 0 else (ch + h' - h) / h_sps

    show' :: Show a => a -> Text
    show' = Text.pack . show

    w = foldr spaceSize 0 $ map fst $ cavity
    h = foldr spaceSize 0 $ map snd $ cavity

    (cwr,chr) = cavityRange mosaic (w,h)

    (cw,ch) = (fst cwr - snd cwr, fst chr - snd chr)

    w_sps = fromIntegral $ length [ () | Space' <- map fst cavity ]
    h_sps = fromIntegral $ length [ () | Space' <- map snd cavity ]


-- how about a version that does not use spacing?
pack :: Mosaic a -> Tile a
pack mosaic@(Mosaic cavity k) = Tile (w,h) $ \ (x,y) (w',h') -> do
      fst $ k (x,y) $ Cavity (0,0) (w',h') $ spacingInMosaic mosaic (w',h')
  where

    w = foldr spaceSize 0 $ map fst $ cavity
    h = foldr spaceSize 0 $ map snd $ cavity

spaceSize :: Spacing' -> Float -> Float
spaceSize (Alloc n)   sz = sz + n
spaceSize (AtLeast n) sz = sz `max` n
spaceSize (Space')    sz = sz
