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

import GHC.Exts (IsString(fromString))

import Graphics.Storyboard.Types
import Graphics.Storyboard.Literals
--import Graphics.Storyboard.TextStyle


------------------------------------------------------------------------

data Prose
      = ProseItem  Text   -- can include fixed space, but will not usually.
      | ProseSpace Float  -- normalize to 1 for regular space
      | ProseScope (TheProseStyle -> TheProseStyle) Prose
      | ProseConcat [Prose]

instance IsString Prose where
  fromString txt = ProseConcat $ List.intersperse (ProseSpace 1)
      [ ProseItem $ Text.pack $ wd -- default is *no* annotations
      | wd <- words txt
      ]

instance Show Prose where
   show (ProseItem txt) = show txt
   show (ProseSpace n) = show n
   show (ProseScope _ p) = "{" ++ show p ++ "}"
   show (ProseConcat ps) = unwords $ map show ps

instance Semigroup Prose where
  xs <> ys = ProseConcat [xs,ys]

instance Monoid Prose where
  mempty = ProseConcat []
  mappend xs ys = ProseConcat [xs,ys]
  mconcat = ProseConcat

{-
mapProse :: ([Emphasis] -> [Emphasis]) -> Prose -> Prose
mapProse f (Prose ps) = Prose $ map g ps where
  g (Right (Word es txt)) = Right (Word (f es) txt)
  g other = other
-}

{-
super :: Prose -> Prose
super (Prose ps) = Prose $ map f ps
  where f (Left n) = Left (n / 0.7)
        f (Right (Word es txt)) = Right (Word (Super:es) txt)
-}
sizedSpace :: Float -> Prose
sizedSpace n = ProseSpace n

space :: Prose
space = sizedSpace 1

(<+>) :: Prose -> Prose -> Prose
p1 <+> p2 = p1 <> space <> p2

br :: Prose
br = sizedSpace (1/0)  -- a bit of a hack

(</>) :: Prose -> Prose -> Prose
p1 </> p2 = p1 <> br <> p2


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


data TheProseStyle = TheProseStyle
  { theFont           :: Text       -- ^ which font, "sans-serif"
  , theFontSize       :: Int        -- ^ how big, 32
  , theSpaceWidth     :: Float      -- ^ size of space, 0.28 * 32
  , isItalic          :: Bool
  , isBold            :: Bool
  , subSuper          :: Int        -- 0 == regular, 1 == super, 2 == super.super, -1 = sub
  , theColor          :: Text       -- ^ current color, black
  , theLigatures      :: [(Text,Text)]
  } deriving Show

defaultProseStyle = TheProseStyle
  { theFont            = "sans-serif"
  , theFontSize        = 32
  , theSpaceWidth      = onePointSpaceWidth * 32
  , isItalic           = False
  , isBold             = False
  , subSuper           = 0
  , theColor           = "black"
  , theLigatures       = []
  }


x = 11

onePointSpaceWidth :: Float
onePointSpaceWidth = 0.26

class ProseStyle a where
  proseStyle   :: (TheProseStyle -> TheProseStyle) -> a -> a

instance ProseStyle TheProseStyle where
  proseStyle   f s = f s

instance ProseStyle Prose where
  proseStyle   _ s = s -- for now

i           :: ProseStyle a =>          a -> a
i             = proseStyle $ \ s -> s { isItalic = True }

b             = proseStyle $ \ s -> s { isBold = True }
b           :: ProseStyle a =>          a -> a

font        :: ProseStyle a => Text ->  a -> a
font        f = proseStyle $ \ s -> s { theFont = f }

fontSize    :: ProseStyle a => Int  ->  a -> a
fontSize    n = proseStyle $ \ s -> s { theFontSize = n, theSpaceWidth = onePointSpaceWidth * fromIntegral n }

big         :: ProseStyle a =>          a -> a
big           = proseStyle $ \ s -> s { theFontSize = ceiling $ fromIntegral (theFontSize s) * 1.2 }

small       :: ProseStyle a =>          a -> a
small         = proseStyle $ \ s -> s { theFontSize = floor   $ fromIntegral (theFontSize s) / 1.2 }

color       :: ProseStyle a => Text ->  a -> a
color       c = proseStyle $ \ s -> s { theColor = c }

plain       :: ProseStyle a =>          a -> a
plain         = proseStyle $ \ s -> s { isItalic = False, isBold = False }

wordSpacing :: ProseStyle a => Float -> a -> a
wordSpacing w = proseStyle $ \ s -> s { theSpaceWidth = w }

ligature    :: ProseStyle a => Text -> Text -> a -> a
ligature  f t = proseStyle $ \ s -> s { theLigatures = (f,t) : theLigatures s }

noLigatures :: ProseStyle a =>          a -> a
noLigatures   = proseStyle $ \ s -> s { theLigatures = [] }

super       :: ProseStyle a =>          a -> a
super         = proseStyle $ \ s -> s { subSuper = subSuper s + 1 }
