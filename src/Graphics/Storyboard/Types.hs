{-# LANGUAGE KindSignatures, GADTs, GeneralizedNewtypeDeriving #-}

module Graphics.Storyboard.Types where

import Control.Applicative
import Control.Monad (liftM2)
import Data.Semigroup
import Data.Text(Text)
import Graphics.Blank (Canvas)


-----------------------------------------------------------------------------

type Size  f = (f,f)
type Coord f = (f,f)

-----------------------------------------------------------------------------

-- | A Tile has a specific, fixed size.
-- When rendered, it is given a specific size to operate inside of,
-- that typically would be *at least* the size of the original fixed size.
-- The tile can choose to put any extra space on the inside or outside
-- of any border, etc.

data Tile a = Tile (Size Float) (Size Float -> Canvas a)

instance Show (Tile a) where
  show (Tile sz _) = show sz

-- | tile requests a specific (minimum) size, and provides
-- a paint routine that takes the *actual* size.
-- The paint routine can assume the canvas starts at (0,0),
-- and is the given size. No masking is done by default.

tile :: Size Float -> (Size Float -> Canvas a) -> Tile a
tile = Tile

tileWidth :: Tile a -> Float
tileWidth (Tile (w,_) _) = w
tileHeight :: Tile a -> Float
tileHeight (Tile (_,h) _) = h


instance Semigroup a => Semigroup (Tile a) where
  (Tile (x1,y1) c1) <> (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $ \ sz ->
        do r1 <- c1 sz
           r2 <- c2 sz -- overlay is the default monoid
           return (r1 <> r2)

instance Monoid a => Monoid (Tile a) where
  mempty = Tile (0,0) (return mempty)
  (Tile (x1,y1) c1) `mappend` (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $ \ sz ->
      do r1 <- c1 sz
         r2 <- c2 sz -- overlay is the default monoid
         return (r1 `mappend` r2)

-----------------------------------------------------------------------------

data Cavity f = Cavity
  { cavityCorner :: Coord f
  , cavitySize   :: Size f
  , cavitySpacer :: Size f  -- take height *or* width, not both
  }
  deriving Show

data Spacing'
  = Alloc Float    -- take up space
  | AtLeast Float  -- be at least this wide
  | Space'         -- space Filler
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

-- Anchored ??
-- Placed ??

data Filler a = Filler
  { fillerSpace :: [(Spacing',Spacing')]
  , runFiller   :: Cavity Float -> Canvas (a,Cavity Float)
  }

instance Functor Filler where
 fmap f m = pure f <*> m

instance Applicative Filler where
 pure a = Filler [] $ \ sz0 -> return (a,sz0)
 Filler fs f <*> Filler xs x = Filler (fs ++ xs) $ \ sz0 -> do
                    (f',sz1) <- f sz0
                    (x',sz2) <- x sz1
                    return (f' x',sz2)

instance Semigroup a => Semigroup (Filler a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Filler a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-----------------------------------------------------------------------------

data MarkupContext = MarkupContext
  {  baseFont    :: Text      -- which font, "sans-serif"
  ,  fontSize    :: Int       -- how big, 10
  ,  spaceWidth  :: Float     -- size of space, 3.0 (perhaps 2.8)
  ,  baseColor   :: Text      -- current color
  ,  baseJust    :: Justify   -- What justification method are we using
  ,  columnWidth :: Float     -- how wide is the current target column
  }

data Justify = JustLeft | JustCenter | JustRight | Justified
  deriving (Eq,Ord,Show)

justify :: Justify -> MarkupContext -> MarkupContext
justify j m = m { baseJust = j }

spaceWidthX :: (Float -> Float) -> MarkupContext -> MarkupContext
spaceWidthX f m = m { spaceWidth = f (spaceWidth m) }

------------------------------------------------------------------------

-- | The Story Monad is intentually transparent. It is just a convenence.

newtype Story a = Story { runStory :: MarkupContext -> Prelude (a,Filler ()) }

instance Functor Story where
 fmap f m = pure f <*> m

instance Applicative Story where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Story where
  return a = Story $ \ _ -> return (a,pure ())
  Story f >>= k = Story $ \ st -> do
    (a,f1) <- f st
    (r,f2) <- runStory (k a) st
    return (r,f1 <> f2)

instance Semigroup a => Semigroup (Story a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (Story a) where
  mempty = pure $ mempty
  mappend = liftM2 mappend


storyContext :: (MarkupContext -> MarkupContext) -> Story a -> Story a
storyContext f (Story g) = (Story $ g . f)

------------------------------------------------------------------------

newtype Prelude a = Prelude { runPrelude :: Canvas a }
  deriving (Functor, Applicative, Monad)

------------------------------------------------------------------------
