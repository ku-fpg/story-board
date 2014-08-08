{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Slide where
{-
  ( Slide
  , slide
  , Prelude(..)  -- for now
  , draw
  , place
  , cavity
  , environment
  , runSlide
  ) where
-}
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
import Graphics.Storyboard.Paragraph
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Prose
import Graphics.Storyboard.Prelude


-----------------------------------------------------------------------------
data TheSlideStyle = TheSlideStyle
  { theParagraphStyle   :: TheParagraphStyle
  , tabStop             :: Float
  , fullSize            :: Size Float
  , theSlideNumber      :: Int
  , theLastSlide        :: Int
  }
  deriving Show

defaultSlideStyle :: TheSlideStyle
defaultSlideStyle = TheSlideStyle
  { theParagraphStyle = defaultParagraphStyle
  , tabStop           = 50
  , fullSize          = (1024,786)
  , theSlideNumber    = 0
  , theLastSlide      = 0
  }

class ParagraphStyle a => SlideStyle a where
  slideStyle :: (TheSlideStyle -> TheSlideStyle) -> a -> a

instance SlideStyle TheSlideStyle where
  slideStyle f s = f s

instance ParagraphStyle TheSlideStyle where
  paragraphStyle f s = s { theParagraphStyle = f (theParagraphStyle s) }

instance ProseStyle TheSlideStyle where
  proseStyle = paragraphStyle . proseStyle

-----------------------------------------------------------------------------
-- | The Slide Monad is intentually transparent. It is just a convenence.

newtype Slide a = Slide { runSlide :: TheSlideStyle -> Size Float -> Prelude (a,Mosaic ()) }

slide :: (TheSlideStyle -> Size Float -> Prelude (a,Mosaic ())) -> Slide a
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

-- ==> askCavityStyle
cavity :: Slide (Size Float)
cavity = Slide $ \ _ sz -> return (sz,pure ())

askSlideStyle :: Slide TheSlideStyle
askSlideStyle = Slide $ \ env _ -> return (env,pure ())


slidePrelude :: Prelude a -> Slide a
slidePrelude m = Slide $ \ cxt st -> do
      a <- m
      return (a,pure ())

{-
instance SlideStyle TheSlideStyle where
  slideStyle f s = f s

instance ParagraphStyle TheSlideStyle where
  paragraphStyle f s = s { theParagraphStyle = f (theParagraphStyle s) }

instance ProseStyle TheSlideStyle where
  proseStyle = paragraphStyle . proseStyle
-}

instance SlideStyle (Slide a) where
  slideStyle f (Slide g) = Slide $ \ cxt sz -> g (f cxt) sz

instance ParagraphStyle (Slide a) where
  paragraphStyle = slideStyle . paragraphStyle

instance ProseStyle (Slide a) where
  proseStyle = slideStyle . proseStyle


draw :: Mosaic () -> Slide ()
draw mosaic = Slide $ \ cxt sz -> return ((),mosaic)

-- | you 'place' a 'Tile' onto the 'Slide', on a specific side of your slide.
place :: Side -> Tile () -> Slide ()
place s = draw . anchor s
