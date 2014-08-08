{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Slide
  ( Slide
  , slide
  , Prelude(..)  -- for now
  , draw
  , place
  , cavity
  , environment
  , runSlide
  ) where

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
import Graphics.Storyboard.Environment
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Prose
import Graphics.Storyboard.Prelude


-----------------------------------------------------------------------------


-----------------------------------------------------------------------------



------------------------------------------------------------------------

-- | The Slide Monad is intentually transparent. It is just a convenence.

newtype Slide a = Slide { runSlide :: Environment -> Size Float -> Prelude (a,Mosaic ()) }

slide :: (Environment -> Size Float -> Prelude (a,Mosaic ())) -> Slide a
slide = Slide

instance Functor Slide where
 fmap f m = pure f <*> m

instance Applicative Slide where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Slide where
  return a = Slide $ \ _ _ -> return (a,pure ())
  Slide f >>= k = Slide $ \ st sz -> do
    (a,f1) <- f st sz
    (r,f2) <- runSlide (k a) st (cavityMaxSize f1 sz)
    return (r,f1 <> f2)

instance Semigroup a => Semigroup (Slide a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (Slide a) where
  mempty = pure $ mempty
  mappend = liftM2 mappend


instance MonadIO Slide where
    liftIO io = Slide $ \ cxt st -> do
      a <- liftIO io
      return (a, pure ())


cavity :: Slide (Size Float)
cavity = Slide $ \ _ sz -> return (sz,pure ())

storyContext :: (Environment -> Environment) -> Slide a -> Slide a
storyContext f (Slide g) = (Slide $ \ cxt sz -> g (f cxt) sz)

environment :: Slide Environment
environment = Slide $ \ env _ -> return (env,pure ())


instance Layout (Slide a) where
  scoped = storyContext

instance ProseStyle (Slide a) where
  proseStyle = storyContext . proseStyle



{-
instance Markup (Slide a) where
--  align :: Alignment -> Slide a -> Slide a
--  align = storyContext . align
  font     = scoped . font
  color    = storyContext . color
  fontSize = storyContext . fontSize
-}

--size :: Float -> Slide a -> Slide a
--size s = storyContext (\ m -> m { baseAlign = j })

{-
-- Pull out the inner Mosaic.
getSlide :: Slide () -> Slide (Mosaic ())
getSlide (Slide f) = Slide $ \ cxt -> do
    ((),Mosaic) <- f cxt
    return (Mosaic, pure ())
-}
{-
anchor tile left
anchor tile left

(tile `on` left)

draw (tile `on` left)
-}

--  draw (tile ?left)



draw :: Mosaic () -> Slide ()
draw mosaic = Slide $ \ cxt sz -> return ((),mosaic)

-- | you 'place' a 'Tile' onto the 'Slide', on a specific side of your slide.
place :: Side -> Tile () -> Slide ()
place s = draw . anchor s

------------------------------------------------------------------------
-- The idea behind the prelude monad is that we can cache
-- answers asked at Prelude time (always about size)
-- by running in simulation mode.

--newtype Prelude a = Prelude { runPrelude :: Canvas a }
--  deriving (Functor, Applicative, Monad, MonadIO)

-- wordWidth :: MarkupContext -> Word -> Prelude Float
-- imageTile :: FilePath -> Prelude (Tile ())
--

------------------------------------------------------------------------
