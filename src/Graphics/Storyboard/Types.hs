{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Types
  ( module Graphics.Storyboard.Types
  , module Graphics.Storyboard.Types.Basic
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
import Control.Lens (makeLenses)

import GHC.Exts (IsString(fromString))

import Graphics.Storyboard.Types.Basic
import Graphics.Storyboard.Literals
import Graphics.Storyboard.Environment
import Graphics.Storyboard.Mosaic

-----------------------------------------------------------------------------


-----------------------------------------------------------------------------



------------------------------------------------------------------------

-- | The Story Monad is intentually transparent. It is just a convenence.

newtype Story a = Story { runStory :: Environment -> Size Float -> Prelude (a,Mosaic ()) }

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

storyContext :: (Environment -> Environment) -> Story a -> Story a
storyContext f (Story g) = (Story $ \ cxt sz -> g (f cxt) sz)

instance Layout (Story a) where
  scoped = storyContext


instance Markup (Story a) where
--  align :: Alignment -> Story a -> Story a
--  align = storyContext . align
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
