{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Markup where

import qualified Data.Text as Text
import Data.Text(Text)
import Data.Semigroup
import Graphics.Blank
import Data.List as List
import Control.Applicative

import Graphics.Storyboard.Types
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling
import Control.Monad.IO.Class

import GHC.Exts (IsString(fromString))

------------------------------------------------------------------------

data Emphasis
  = Italics
  | Bold
  | Color Text       -- not supported yet
  | Font Int Text    -- not supported yet


instance Show Emphasis where
  show (Italics)     = "i"
  show (Bold)        = "b"
  show (Color col)   = "#" ++ show col
  show (Font sz txt) = show sz ++ "-" ++ show txt

------------------------------------------------------------------------

data Word
      = Word [Emphasis] Text
      | WordSpace Float      -- 1 for space, 0 for (breakable) 0-width-space

instance Show Word where
   show (Word [] txt)   = show $ Text.unpack txt
   show (Word emph txt) = show emph ++ show (Text.unpack txt)
   show (WordSpace n)       = show n

------------------------------------------------------------------------

newtype Prose = Prose [Word]
  deriving Show


instance IsString Prose where
  fromString txt = Prose $ List.intersperse (WordSpace 1)
      [ Word [] (Text.pack wd) -- default is *no* annotations
      | wd <- words txt
      ]


instance Semigroup Prose where
  (Prose xs) <> (Prose ys) = Prose (xs++ys)

instance Monoid Prose where
  mempty = Prose []
  mappend (Prose xs) (Prose ys) = Prose (xs++ys)


sp :: Float -> Prose
sp n = Prose [WordSpace n]

space :: Prose
space = sp 1

(<+>) :: Prose -> Prose -> Prose
p1 <+> p2 = p1 <> space <> p2

------------------------------------------------------------------------

data MarkupContext = MarkupContext
  {  baseFont    :: Text      -- which font, "sans-serif"
  ,  fontSize    :: Int       -- how big, 10
  ,  spaceWidth  :: Float     -- size of space, 3.0 (perhaps 2.8)
  ,  baseColor   :: Text      -- current color
  ,  baseJust    :: Justify   -- What justification method are we using
  ,  columnWidth :: Float     -- how wide is the current column
  }

data Justify = JustLeft | JustCenter | JustRight | Justified

------------------------------------------------------------------------

tileProse :: MarkupContext -> Prose -> Canvas (Filler ())
tileProse cxt (Prose xs) = do

    -- get all the tiles and spaces
    proseTiles <- sequence
        [ case x of
            Word emph txt -> do
              font $ emphasisFont (baseFont cxt) emph
              TextMetrics w <- measureText txt
              return $ Right $ tile (w,fromIntegral $ fontSize cxt + 5) $ const $ do
                font $ emphasisFont (baseFont cxt) emph
                fillStyle (baseColor cxt)
                fillText (txt,0,fromIntegral $ fontSize cxt)    -- time will tell for this offset
            WordSpace n -> return $ Left $ n * spaceWidth cxt
        | x <- xs
        ]

    liftIO $ sequence_
            [ case v of
                Left n -> print ("SP",n)
                Right t -> print ("T",tileWidth t)
            | v <- proseTiles
            ]

    let
        findT (Left n:xs) ts = findS xs (ts,n)
        findT (Right t:xs) ts = findT xs (ts++[t])
        findT [] ts = [(ts,0)]

        findS (Left n:xs) (ts,w) = findS xs (ts,w + n)
        findS (Right t:xs) (ts,w) = (ts,w) : findT xs [t]
        findS [] (ts,w) = [(ts,w)]


    let glyphs2 :: [([Tile ()],Float)] = findT proseTiles []

    liftIO $ sequence_
        [ print (map tileWidth ts,w)
        | (ts,w) <- glyphs2
        ]

    let splits = splitLines (columnWidth cxt) [ (sum $ map tileWidth ts,w) | (ts,w) <- glyphs2 ]


    liftIO $ print $ splits
    -- now finally laydown the tiles

    let
        write :: [([Tile ()],Float)] -> Filler ()
        write xs = top $ pack $ mconcat
              [ mconcat [ left $ border 1 "red" $ tile | tile <- tiles ] <>
                (if sp == 0
                 then pure ()
                 else left $ tile (sp,0) $ const $ return ()
                )
              | (tiles,sp) <- xs
              ]

        loop []     [] = pure ()
        loop (n:ns) xs =
            write (take n xs) <>
            loop ns (drop n xs)

    return $ loop splits glyphs2

------------------------------------------------------------------------

newtype Paragraph = Paragraph [Word]

instance Show Paragraph where
  show (Paragraph wds) = unwords [ show $ Text.unpack wd | Word _ wd <- wds ]

instance IsString Paragraph where
  fromString txt = Paragraph
      [ Word [] (Text.pack wd) -- default is *no* annotations
      | wd <- words txt
      ]


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
splitLine' :: Float -> Float -> [Float] -> Int
splitLine' spaceWidth lineWidth widths = length $ takeWhile (<= lineWidth) szs
  where
    szs = [ sz + sp + rest
          | (sz,sp,rest) <-
                zip3 widths
                     (0 : repeat spaceWidth) -- spaces count from 2nd word on
                     (0 : szs)
          ]

-- Given the (min) width of a space, the width of the line,
-- and a list of word widths, how many words can we accept.
splitLine :: Float -> [(Float,Float)] -> Int
splitLine lineWidth widths = length $ takeWhile (<= lineWidth) szs
  where
    szs = [ sz + sp + rest
          | (sz,sp,rest) <-
                zip3 (map fst widths)
                     (0 : map snd widths)
                     (0 : szs)
          ]

splitLines :: Float -> [(Float,Float)] -> [Int]
splitLines lineWidth [] = []
splitLines lineWidth xs = n : splitLines lineWidth (drop n xs)
  where
    n = splitLine lineWidth xs `max` 1 -- hfill warning here


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
