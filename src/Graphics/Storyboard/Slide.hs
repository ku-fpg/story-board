{-# LANGUAGE KindSignatures, TupleSections, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

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
import Graphics.Blank (Canvas,saveRestore,sync)
import Control.Monad.IO.Class
import Control.Concurrent as Concurrent

import Graphics.Blank(EventQueue)

import GHC.Exts (IsString(fromString))

import Graphics.Storyboard.Act (Behavior)
import Graphics.Storyboard.Types
import Graphics.Storyboard.Literals
import Graphics.Storyboard.Paragraph
import Graphics.Storyboard.Prose
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Box
import Graphics.Storyboard.Prose
import qualified Graphics.Storyboard.Prelude as Prelude
import Graphics.Storyboard.Prelude (Prelude,wordWidth)



-----------------------------------------------------------------------------
data TheSlideStyle = TheSlideStyle
  { theProseStyle       :: TheProseStyle
  , theParagraphStyle   :: TheParagraphStyle
  , theBoxStyle         :: TheBoxStyle
  , theItemCounters     :: [Int]
  , theBulletFactory    :: BulletFactory
  , theTabStop          :: Float
  , fullSize            :: Size Float
  , theSlideNumber      :: Int
  , theLastSlide        :: Int
  , theEventQueue       :: TheEventQueue
  }
  deriving Show

newtype BulletFactory = BulletFactory
  { runBulletFactory :: Int -> [Int] -> Bullet
  }

instance Show BulletFactory where
  show _ = "BulletFactory{}"


defaultSlideStyle :: EventQueue -> Size Float -> TheSlideStyle
defaultSlideStyle eventQ sz = TheSlideStyle
  { theProseStyle     = defaultProseStyle
  , theParagraphStyle = defaultParagraphStyle
  , theBoxStyle       = defaultBoxStyle
  , theItemCounters   = []
  , theBulletFactory  = defaultBulletFactory
  , theTabStop        = 50
  , fullSize          = sz
  , theSlideNumber    = 0
  , theLastSlide      = 0
  , theEventQueue     = TheEventQueue eventQ
  }

defaultBulletFactory :: BulletFactory
defaultBulletFactory = BulletFactory $ \ _ _ -> bulletText "\x2022 "


newtype TheEventQueue = TheEventQueue EventQueue

instance Show TheEventQueue where show _ = "TheEventQueue{}"

class (ProseStyle a, ParagraphStyle a, BoxStyle a) => SlideStyle a where
  slideStyle :: (TheSlideStyle -> TheSlideStyle) -> a -> a

--instance ProseStyle TheParagraphStyle where
--  proseStyle f e = e { theProseStyle = f (theProseStyle e) }

instance SlideStyle TheSlideStyle where
  slideStyle f s = f s

instance ParagraphStyle TheSlideStyle where
  paragraphStyle f s = s { theParagraphStyle = f (theParagraphStyle s) }

instance ProseStyle TheSlideStyle where
  proseStyle f s = s { theProseStyle = f (theProseStyle s) }

instance BoxStyle TheSlideStyle where
  boxStyle f s = s { theBoxStyle = f (theBoxStyle s) }


consItemCounters :: SlideStyle a => Int -> a -> a
consItemCounters n = slideStyle $ \ m -> m { theItemCounters = n : theItemCounters m }

bulletFactory :: SlideStyle a => BulletFactory -> a -> a
bulletFactory fac = slideStyle $ \ m -> m { theBulletFactory = fac }

--localItemCount :: SlideState a => a ->
--localItemCount

-----------------------------------------------------------------------------

data TheSlideState = TheSlideState
  { theMosaic        :: Mosaic ()
  , previousMosaics :: [Mosaic ()]
  , theInternalSize  :: Size Float
  , theItemCounter   :: Int
  }
  deriving Show

defaultSlideState :: Size Float -> TheSlideState
defaultSlideState sz = TheSlideState
  { theMosaic        = (hbrace $ fst $ sz)
                    <> (vbrace $ snd $ sz)
  , previousMosaics = []
  , theInternalSize  = sz
  , theItemCounter   = 0
  }

drawMosaic :: Mosaic () -> TheSlideState -> TheSlideState
drawMosaic moz st = st
  { theMosaic = theMosaic st <> moz
  , theInternalSize = cavityMaxSize moz (theInternalSize st)
  }

replaceMosaic :: Mosaic () -> TheSlideState -> TheSlideState
replaceMosaic moz st = st
    { theMosaic = moz
    , previousMosaics = theMosaic st : previousMosaics st
    , theInternalSize = cavityMaxSize moz (theInternalSize st)
    }

incItemCount :: TheSlideState -> TheSlideState
incItemCount m = m { theItemCounter = 1 + theItemCounter m }

setItemCount :: Int -> TheSlideState -> TheSlideState
setItemCount n m = m { theItemCounter = n }


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

getCavitySize :: Slide (Size Float)
getCavitySize = Slide $ \ _ st -> return (theInternalSize st,st)

askSlideStyle :: Slide TheSlideStyle
askSlideStyle = Slide $ \ env st -> return (env,st)

getSlideState :: Slide TheSlideState
getSlideState = Slide $ \ _ st -> return (st,st)

setSlideState :: TheSlideState -> Slide ()
setSlideState st = Slide $ \ _ _ -> return ((),st)

modSlideState :: (TheSlideState -> TheSlideState) -> Slide ()
modSlideState f = Slide $ \ _ st -> return ((),f st)

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

instance BoxStyle (Slide a) where
  boxStyle = slideStyle . boxStyle


-- Draw a mosaic onto a slide
draw :: Mosaic () -> Slide ()
draw moz = Slide $ \ cxt st -> return ((),drawMosaic moz st)

-- | you 'place' a 'Tile' onto the 'Slide', on a specific side of your slide.
place :: Side -> Tile () -> Slide ()
place s = draw . anchor s

pause :: Slide ()
pause = Slide $ \ cxt st -> do
  let cavity = cavityOfMosaic (theMosaic st) (fullSize cxt)
  liftIO $ print (cavity,"Cavity")
{-
let Tile (w,h) m = fillTile (theMosaic st)
  ((),cavity) <- Prelude.liftCanvas $ saveRestore $ do
      m (0,0) (w,h)

  () <- liftCanvas sync

  liftIO $ putStrLn "pausing"
  Prelude.keyPress
--  liftIO $ Concurrent.threadDelay (1 * 1000 * 1000)
  liftIO $ putStrLn "paused"
-}

  let currBorder = blankMosaic (fullSize cxt) cavity

  return ((),replaceMosaic currBorder st)

-------------------------------------------------------------------------
-- | 'tileOfSide' creates a tile of a sub-slide, of a specified size.

tileOfSlide :: Size Float -> Slide () -> Slide (Tile ())
tileOfSlide sz (Slide f) = Slide $ \ slide_style slide_state -> do
    let slide_style0 = slide_style
          { theItemCounters = []
          , theParagraphStyle = defaultParagraphStyle
          , theBulletFactory = defaultBulletFactory
          }
    let slide_state0 = defaultSlideState sz

    (a, slide_state1) <- f slide_style0 slide_state0

    return (pack (theMosaic slide_state1),slide_state)

-------------------------------------------------------------------------
-- | 'trueSpace' computes the spacing value, based on the actual width inside the font

trueSpace :: Slide a -> Slide a
trueSpace (Slide f) = Slide $ \ cxt st -> do
  let sz = theFontSize $ theProseStyle cxt
  w <- wordWidth (fontName $ theProseStyle cxt) " "
  f (wordSpacing (w / fromIntegral sz) cxt) st

bgLinear :: Color -> Color -> Slide Background
bgLinear c0 c1 = Slide $ \ _ st -> fmap (,st) $ Prelude.bgLinear c0 c1
