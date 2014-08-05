{-# LANGUAGE KindSignatures, GADTs, GeneralizedNewtypeDeriving, InstanceSigs #-}

module Graphics.Storyboard.Types where

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

-- Short cut literal/DSL-keyword classes
class LR a where
  left :: a
  right :: a

class TB a where
  top :: a
  bottom :: a

class Center a where
  center :: a

-----------------------------------------------------------------------------

data Side       = T | B | L | R
  deriving (Eq,Ord,Show)

instance LR Side where
  left = L
  right = R

instance TB Side where
  top = T
  bottom = B

-----------------------------------------------------------------------------

data Vertical   = VT | VC | VB
  deriving (Eq,Ord,Show)

instance TB Vertical where
  top = VT
  bottom = VB

instance Center Vertical where
  center = VC

-----------------------------------------------------------------------------

data Horizontal = HL | HC | HR
  deriving (Eq,Ord,Show)

instance LR Horizontal where
  left = HL
  right = HR

instance Center Horizontal where
  center = HC

-----------------------------------------------------------------------------
-- http://en.wikipedia.org/wiki/Typographic_alignment

data Alignment = JustLeft | JustCenter | JustRight | Justified
  deriving (Eq,Ord,Show)

instance LR Alignment where
  left = JustLeft
  right = JustRight

instance Center Alignment where
  center = JustCenter

justified :: Alignment
justified = Justified

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

data MarkupContext = MarkupContext
  {  baseFont    :: Text      -- which font, "sans-serif"
  ,  fontSize    :: Int       -- how big, 10
  ,  spaceWidth  :: Float     -- size of space, 3.0 (perhaps 2.8)
  ,  baseColor   :: Text      -- current color
  ,  baseAlign   :: Alignment -- What alignment method are we using
  ,  baseOffset  :: Float     -- size of offset from baseline
  }
  deriving (Show)

spaceWidthX :: (Float -> Float) -> MarkupContext -> MarkupContext
spaceWidthX f m = m { spaceWidth = f (spaceWidth m) }

class Context a where
  align :: Alignment -> a -> a    -- might be seperate, because of prose
--  font_ :: Text      -> a -> a
  raisebox :: Float -> a -> a
--  sub   :: a -> a
  color :: Text      -> a -> a
  size  :: Int       -> a -> a
  context :: (MarkupContext -> a) -> a -> a

instance Context MarkupContext where
  align j m = m { baseAlign = j }
  color c m = m { baseColor = c }
  raisebox o m = m { baseOffset = baseOffset m + o }
  size  i m = m { fontSize = i, spaceWidth = fromIntegral i * 0.28 }
  context f m = f m

------------------------------------------------------------------------

-- | The Story Monad is intentually transparent. It is just a convenence.

newtype Story a = Story { runStory :: MarkupContext -> Size Float -> Prelude (a,Mosaic ()) }

instance Functor Story where
 fmap f m = pure f <*> m

instance Applicative Story where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Story where
  return a = Story $ \ _ _ -> return (a,pure ())
  Story f >>= k = Story $ \ st sz -> do
    (a,f1) <- f st sz
    (r,f2) <- runStory (k a) st (cavityMaxSize f1 sz)
    return (r,f1 <> f2)

instance Semigroup a => Semigroup (Story a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (Story a) where
  mempty = pure $ mempty
  mappend = liftM2 mappend


instance MonadIO Story where
    liftIO io = Story $ \ cxt st -> do
      a <- liftIO io
      return (a, pure ())


storyCavity :: Story (Size Float)
storyCavity = Story $ \ _ sz -> return (sz,pure ())

storyContext :: (MarkupContext -> MarkupContext) -> Story a -> Story a
storyContext f (Story g) = (Story $ \ cxt sz -> g (f cxt) sz)


instance Context (Story a) where
  align :: Alignment -> Story a -> Story a
  align = storyContext . align
  color = storyContext . color
  size  = storyContext . size

--size :: Float -> Story a -> Story a
--size s = storyContext (\ m -> m { baseAlign = j })

{-
-- Pull out the inner Mosaic.
getStory :: Story () -> Story (Mosaic ())
getStory (Story f) = Story $ \ cxt -> do
    ((),Mosaic) <- f cxt
    return (Mosaic, pure ())
-}

storyMosaic :: Mosaic () -> Story ()
storyMosaic mosaic = Story $ \ cxt sz -> return ((),mosaic)

------------------------------------------------------------------------
-- The idea behind the prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in simulation mode.

newtype Prelude a = Prelude { runPrelude :: Canvas a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- wordWidth :: MarkupContext -> Word -> Prelude Float
-- imageTile :: FilePath -> Prelude (Tile ())
--

------------------------------------------------------------------------

------------------------------------------------------------------------

-- [font style] [font weight] [font size] [font face]
-- style = normal | italic | oblique | inherit
-- weight = normal | bold | bolder | lighter | 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900 | inherit | auto
-- size = 24px (must be an int)
-- face =  serif, sans-serif, cursive, fantasy, and monospace.

data Emphasis
  = Italics
  | Bold
  | Color Text       -- not supported yet
  | Font Int Text    -- not supported yet
  | Sub
  | Super
    deriving (Eq,Ord)

instance Show Emphasis where
  show (Italics)     = "i"
  show (Bold)        = "b"
  show (Color col)   = ":" ++ show col
  show (Font sz txt) = show sz ++ "-" ++ show txt
  show (Sub)         = "_"
  show (Super)       = "^"

------------------------------------------------------------------------

data Word
      = Word [Emphasis] Text
--      | WordSpace Float      -- 1 for space, 0 for (breakable) 0-width-space

instance Show Word where
   show (Word [] txt)   = show $ Text.unpack txt
   show (Word emph txt) = show emph ++ show (Text.unpack txt)
--   show (WordSpace n)   = show n


------------------------------------------------------------------------

newtype Prose = Prose [Either Float Word]
  deriving Show


instance IsString Prose where
  fromString txt = Prose $ List.intersperse (Left 1)
      [ Right $ Word [] $ Text.pack $ wd -- default is *no* annotations
      | wd <- words txt
      ]

instance Semigroup Prose where
  (Prose xs) <> (Prose ys) = Prose (xs++ys)

instance Monoid Prose where
  mempty = Prose []
  mappend (Prose xs) (Prose ys) = Prose (xs++ys)

super :: Prose -> Prose
super (Prose ps) = Prose $ map f ps
  where f (Left n) = Left (n / 0.7)
        f (Right (Word es txt)) = Right (Word (Super:es) txt)
