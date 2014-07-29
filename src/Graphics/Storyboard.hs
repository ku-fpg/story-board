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

spaceSize :: Spacing -> Float -> Float
spaceSize (Alloc n)   sz = sz + n
spaceSize (AtLeast n) sz = sz `max` n
spaceSize (Space)     sz = sz

-----------------------------------------------------------------------------

-- | tile requests a specific (minimum) size, and provides
-- a paint routine that takes the *actual* size.
-- The paint routine can assume the canvas starts at (0,0),
-- and is the given size. No masking is done by default.

tile :: (Float,Float) -> ((Float,Float) -> Canvas a) -> Tile a
tile = Tile

--center :: Tile a -> Tile a
center t = hfill *> left t <* hfill

class Place f where
    left   :: Tile a -> f a
    right  :: Tile a -> f a
    top    :: Tile a -> f a
    bottom :: Tile a -> f a
    pack   :: f a -> Tile a

instance Place Filling where
    left = tileLeft

instance Place Tile where
    left t = fillTile (left t <* hfill)

column :: [Tile ()] -> Tile ()
column xs = fillTile $ mconcat $ intersperse vfill $ map tileLeft $ xs
row    :: [Tile a] -> Tile a
row = undefined

-----------------------------------------------------------------------------


vfill :: Filling ()
vfill = Filling [(Space,Alloc 0)] $
  \ (Cavity (cx',cy') (cw',ch') spaces@(sw,sh)) -> do
      return ((),Cavity (cx' + sw,cy') (cw' - sw,ch') spaces)


hfill :: Filling ()
hfill = Filling [(Alloc 0,Space)] $
  \ (Cavity (cx',cy') (cw',ch') spaces@(sw,sh)) -> do
      return ((),Cavity (cx',cy' + sh) (cw',ch' - sh) spaces)


tileTop :: Tile a -> Filling a
tileTop (Tile (w,h) k) = Filling [(AtLeast w,Alloc h)] $
    \ (Cavity (cx',cy') (cw',ch') spaces) -> do
        a <- saveRestore $ do
                translate (cx',cy')
                k (cw',h)
        return (a,Cavity (cx',cy' + h) (cw',ch' - h) spaces)

tileLeft :: Tile a -> Filling a
tileLeft (Tile (w,h) k) = Filling [(Alloc w, AtLeast h)] $
    \ (Cavity (cx',cy') (cw',ch') spaces) -> do
        a <- saveRestore $ do
                translate (cx',cy')
                k (w,ch')
        return (a,Cavity (cx' + w,cy') (cw' - w,ch') spaces)

box = fillTile
fillTile :: Filling a -> Tile a
fillTile (Filling cavity k) = Tile (w,h) $ \ (w',h') -> do
      let sw = if w' <= w then 0 else (w' - w) / w_sps
      let sh = if h' <= h then 0 else (h' - h) / h_sps
      fst <$> k (Cavity (0,0) (w',h') (sw,sh))
  where
    w = foldr spaceSize 0 $ map fst $ cavity
    h = foldr spaceSize 0 $ map snd $ cavity
    w_sps = fromIntegral $ length [ () | Space <- map fst cavity ]
    h_sps = fromIntegral $ length [ () | Space <- map snd cavity ]

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


example2 :: Filling ()
example2 =
  hfill *>
  (tileTop $ example1 "red")    *>
  (tileTop $ column [ example1 "red" | i <- [0..10]]) *>
  (tileTop $ example1 "green")  *>
  (tileLeft $ example1 "pink")    *> vfill *>
  (tileLeft$ example1 "blue")  *>
  (tileTop $ example1 "orange")  *>
  (tileLeft $ example1 "pink")    *>
  (tileLeft$ example1 "blue")  *> vfill *>
  (tileLeft $ example1 "pink")    *>
  (tileLeft$ example1 "blue")  *>
  pure ()

main = do
    print cavity
    print $ foldr spaceSize 0 $ map fst $ cavity
    print $ foldr spaceSize 0 $ map snd $ cavity
--    putStrLn $ unlines $ map prettyEq $ nub eqs
  where
    Filling cavity _ = example2

--          Tile _ m = fillTile (width context - 200,height context - 200) $ example2

main2 = blankCanvas 3000 $ \ context -> do
      send context $ do
        fillStyle "orange"
        translate (100,100)
        let Tile (w,h) m = fillTile example2
        _ <- m (w + 100,h + 100)
        return ()
