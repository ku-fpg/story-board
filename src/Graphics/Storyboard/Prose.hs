{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Prose where

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

mapProse :: ([Emphasis] -> [Emphasis]) -> Prose -> Prose
mapProse f (Prose ps) = Prose $ map g ps where
  g (Right (Word es txt)) = Right (Word (f es) txt)
  g other = other


super :: Prose -> Prose
super (Prose ps) = Prose $ map f ps
  where f (Left n) = Left (n / 0.7)
        f (Right (Word es txt)) = Right (Word (Super:es) txt)

sizedSpace :: Float -> Prose
sizedSpace n = Prose [Left n]

space :: Prose
space = sizedSpace 1

(<+>) :: Prose -> Prose -> Prose
p1 <+> p2 = p1 <> space <> p2

br :: Prose
br = sizedSpace (1/0)  -- a bit of a hack

(</>) :: Prose -> Prose -> Prose
p1 </> p2 = p1 <> br <> p2

instance Markup Prose where
  i :: Prose -> Prose
  i = mapProse (Italics :)

  b :: Prose -> Prose
  b = mapProse (Bold :)

  plain :: Prose -> Prose
  plain = mapProse (const [])


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

data Word = Word [Emphasis] Text

instance Show Word where
   show (Word [] txt)   = show $ Text.unpack txt
   show (Word emph txt) = show emph ++ show (Text.unpack txt)

------------------------------------------------------------------------
