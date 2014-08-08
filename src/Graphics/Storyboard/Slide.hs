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

defaultSlideStyle :: Size Float -> TheSlideStyle
defaultSlideStyle sz = TheSlideStyle
  { theParagraphStyle = defaultParagraphStyle
  , tabStop           = 50
  , fullSize          = sz
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

data TheSlideState = TheSlideState
  { theMosaic        :: Mosaic ()
  , theInternalSize  :: Size Float
  , theSectionCount  :: [Int]
  }

defaultSlideState :: TheSlideStyle -> TheSlideState
defaultSlideState env = TheSlideState
  { theMosaic        = pure ()
  , theInternalSize  = fullSize env
  , theSectionCount  = []
  }

drawMosaic :: Mosaic () -> TheSlideState -> TheSlideState
drawMosaic moz st = st { theMosaic = theMosaic st <> moz
                       , theInternalSize = cavityMaxSize moz (theInternalSize st)
                       }


-----------------------------------------------------------------------------
-- | The Slide Monad is intentually transparent. It is just a convenence.

newtype Slide a = Slide { runSlide :: TheSlideStyle -> TheSlideState -> Prelude (a,TheSlideState) }

slide :: (TheSlideStyle -> TheSlideState -> Prelude (a,TheSlideState)) -> Slide a
slide = Slide

instance Functor Slide where
 fmap f m = pure f <*> m

instance Applicative Slide where
  pure = return
  f <*> a = liftM2 ($) f a

instance Monad Slide where
  return a = Slide $ \ _ st -> return (a,st)
  Slide f >>= k = Slide $ \ env st0 -> do
    (a,st1) <- f env st0
    (r,st2) <- runSlide (k a) env st1
    return (r,st2)

instance Semigroup a => Semigroup (Slide a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (Slide a) where
  mempty = pure $ mempty
  mappend = liftM2 mappend

instance MonadIO Slide where
    liftIO = slidePrelude . liftIO


cavity = getCavitySize

getCavitySize :: Slide (Size Float)
getCavitySize = Slide $ \ _ st -> return (theInternalSize st,st)

askSlideStyle :: Slide TheSlideStyle
askSlideStyle = Slide $ \ env st -> return (env,st)

slidePrelude :: Prelude a -> Slide a
slidePrelude m = Slide $ \ cxt st -> do
      a <- m
      return (a,st)


instance SlideStyle (Slide a) where
  slideStyle f (Slide g) = Slide $ \ cxt sz -> g (f cxt) sz

instance ParagraphStyle (Slide a) where
  paragraphStyle = slideStyle . paragraphStyle

instance ProseStyle (Slide a) where
  proseStyle = slideStyle . proseStyle


-- Draw a mosaic onto a slide
draw :: Mosaic () -> Slide ()
draw moz = Slide $ \ cxt st -> return ((),drawMosaic moz st)

-- | you 'place' a 'Tile' onto the 'Slide', on a specific side of your slide.
place :: Side -> Tile () -> Slide ()
place s = draw . anchor s
