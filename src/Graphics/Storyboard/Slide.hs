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

import           Control.Applicative
import           Control.Monad (liftM2)
import           Control.Monad.IO.Class
import           System.Directory

import           Data.Semigroup

import           Graphics.Blank (EventQueue, newCanvas, with, toDataURL,writeDataURL)
import           Graphics.Storyboard.Act
import           Graphics.Storyboard.Box
import           Graphics.Storyboard.Deck
import           Graphics.Storyboard.Images
import           Graphics.Storyboard.Literals
import           Graphics.Storyboard.Mosaic
import           Graphics.Storyboard.Paragraph
import qualified Graphics.Storyboard.Prelude as Prelude
import           Graphics.Storyboard.Prelude (Prelude, wordWidth)
import           Graphics.Storyboard.Prose
import           Graphics.Storyboard.Tile
import           Graphics.Storyboard.Types

-----------------------------------------------------------------------------

data TheSlideStyle = TheSlideStyle
  { theProseStyle       :: TheProseStyle
  , theParagraphStyle   :: TheParagraphStyle
  , theBoxStyle         :: TheBoxStyle
  , theItemCounters     :: [Int]
  , theBulletFactory    :: BulletFactory
  , theTabStop          :: Double
  , fullSize            :: Size Double
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


defaultSlideStyle :: EventQueue -> Size Double -> TheSlideStyle
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
  , theInternalSize  :: Size Double
  , theDeck          :: Deck
  , theItemCounter   :: Int
  }
  deriving Show

defaultSlideState :: Size Double -> TheSlideState
defaultSlideState sz = TheSlideState
  { theMosaic        = (hbrace $ fst $ sz)
                    <> (vbrace $ snd $ sz)
  , previousMosaics = []
  , theInternalSize  = sz
  , theDeck          = defaultDeck sz
  , theItemCounter   = 0
  }

drawMosaic :: Mosaic () -> TheSlideState -> TheSlideState
drawMosaic moz st = st
  { theMosaic = theMosaic st <> moz
  , theInternalSize = cavityMaxSize moz (theInternalSize st)
  , theDeck = deck
  } where deck = drawOnDeck moz (theDeck st)

-- To remove
replaceMosaic :: Mosaic () -> TheSlideState -> TheSlideState
replaceMosaic moz st = st
    { theMosaic = moz
    , previousMosaics = theMosaic st : previousMosaics st
    , theInternalSize = cavityMaxSize moz (theInternalSize st)
    }

pauseSlide :: TheSlideState -> TheSlideState
pauseSlide st = st
  { theDeck = pauseDeck (theDeck st)
  }


incItemCount :: TheSlideState -> TheSlideState
incItemCount m = m { theItemCounter = 1 + theItemCounter m }

setItemCount :: Int -> TheSlideState -> TheSlideState
setItemCount n m = m { theItemCounter = n }


-----------------------------------------------------------------------------
-- | The Slide Monad is intentionally transparent. It is just a convenience.

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

instance MonadCanvas Slide where
  liftCanvas = slidePrelude . liftCanvas

getCavitySize :: Slide (Size Double)
getCavitySize = Slide $ \ _ st -> return (cavitySize $ deckCavity $ theDeck st,st)

askSlideStyle :: Slide TheSlideStyle
askSlideStyle = Slide $ \ env st -> return (env,st)

getSlideState :: Slide TheSlideState
getSlideState = Slide $ \ _ st -> return (st,st)

setSlideState :: TheSlideState -> Slide ()
setSlideState st = Slide $ \ _ _ -> return ((),st)

modSlideState :: (TheSlideState -> TheSlideState) -> Slide ()
modSlideState f = Slide $ \ _ st -> return ((),f st)

slidePrelude :: Prelude a -> Slide a
slidePrelude m = Slide $ \ _ st -> do
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
draw moz = Slide $ \ _ st -> return ((),drawMosaic moz st)

-- | you 'place' a 'Tile' onto the 'Slide', on a specific side of your slide.
place :: Side -> Tile () -> Slide ()
place s = draw . anchor s

pause :: Slide ()
pause = Slide $ \ _ st -> return ((),pauseSlide st)

-------------------------------------------------------------------------
-- | 'tileOfSide' creates a tile of a sub-slide, of a specified size.

tileOfSlide :: Size Double -> Slide () -> Slide (Tile ())
tileOfSlide sz (Slide f) = Slide $ \ slide_style slide_state -> do
    let slide_style0 = slide_style
          { theItemCounters = []
          , theParagraphStyle = defaultParagraphStyle
          , theBulletFactory = defaultBulletFactory
          }
    let slide_state0 = defaultSlideState sz

    (_, slide_state1) <- f slide_style0 slide_state0

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


-------------------------------------------------------------------------

-- store a tile in a named cache location, and use the cache if it exists.

cacheTile :: FilePath -> Tile () -> Slide (Tile ())
cacheTile fileName tile@(Tile (w,h) act) = do
    b <- liftIO $ doesFileExist fileName
    if b then do
      imageTile fileName
    else case getScenery $ act (Cavity (0,0) (w,h)) of
      Nothing -> return tile
      Just m -> do
        liftIO $ print $ "pre-record the tile"
        url <- liftCanvas $ do
                  cvs <- newCanvas (round w,round h)
                  with cvs $ do
                    m
                    toDataURL()
        liftIO $ writeDataURL fileName url
        return tile
