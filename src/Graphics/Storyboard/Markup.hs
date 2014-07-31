module Graphics.Storyboard.Markup where

import qualified Data.Text as Text
import Data.Text(Text)
import Data.Monoid
import Graphics.Blank
import Data.List as List

import Graphics.Storyboard.Types
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling

import GHC.Exts (IsString(fromString))

data Emphasis
  = Italics
  | Bold
  | Color Text       -- not supported yet
  | Font Int Text    -- not supported yet
  deriving Show

data Word = Word [Emphasis] Text
  deriving Show

newtype Paragraph = Paragraph [Word]

instance Show Paragraph where
  show (Paragraph wds) = unwords [ show $ Text.unpack wd | Word _ wd <- wds ]

instance IsString Paragraph where
  fromString txt = Paragraph
      [ Word [] (Text.pack wd) -- default is *no* annotations
      | wd <- words txt
      ]

data MarkupContext = MarkupContext
  {  baseFont   :: Text      -- which font, "sans-serif"
  ,  fontSize   :: Int       -- how big, 10
  ,  spaceWidth :: Float     -- size of space, 3.0 (perhaps 2.8)
  }

{- Notes about spaces
   (from http://www.microsoft.com/typography/developers/fdsspec/spaces.aspx)

Advance width rule :
  The space's advance width is set by visually selecting a value that is
  appropriate for the current font. The general guidelines for the advance
  widths are:
    * The minimum value should be no less than 1/5 the em, which is equivalent
      to the value of a thin space in traditional typesetting.
    * For an average width font a good value is ~1/4 the em.
 -}

-- figure out the font for this word
emphasisFont :: Text -> [Emphasis] -> Text
emphasisFont baseFont emph = Text.intercalate " " $
    [ t | e <- emph, Just t <- [f e] ] ++ [baseFont]
  where
    f :: Emphasis -> Maybe Text
    f Italics   = return "italics"
    f Bold      = return "bold"
    f _         = fail "no match"

-- This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: Text -> Word -> Canvas Float
wordWidth baseFont (Word emph txt) = saveRestore $ do
  font (emphasisFont baseFont emph)
  TextMetrics w <- measureText txt
  return w

-- a tile has a height and width. The assumption is that the top of the
wordTile :: Text -> Word -> Canvas (Tile ())
wordTile baseFont wd@(Word emph txt) = do
  wd_w <- wordWidth baseFont wd
  return $ border 1 "red" $ tile (wd_w,20) $ const $ do
    font $ emphasisFont baseFont emph
    fillStyle "black"
    fillText (txt,0,10)

-- Given the (min) width of a space, the width of the line,
-- and a list of word widths, how many words can we accept.
splitLine :: Float -> Float -> [Float] -> Int
splitLine spaceWidth lineWidth widths = length $ takeWhile (<= lineWidth) szs
  where
    szs = [ sz + sp + rest
          | (sz,sp,rest) <-
                zip3 widths
                     (0 : repeat spaceWidth) -- spaces count from 2nd word on
                     (0 : szs)
          ]

data Justify = JustLeft | JustCenter | JustRight | Justified

layoutLine :: (Float,Float) -> Text -> Float -> Justify -> [(Word,Float)] -> Tile ()
layoutLine (w,h) baseFont spaceWidth JustLeft theWords = border 1 "blue" $
    pack $ mconcat $ List.intersperse spaceTile $
      [ left $ border 1 "red" $ tile (wd_w,h) $ const $ do
          font $ emphasisFont baseFont emph
          fillStyle "black"
          fillText (wd,0,10)    -- time will tell for this offset
      | (Word emph wd,wd_w) <- theWords
      ]
 where spaceTile = left $ tile (spaceWidth,h) $ const $ return ()
