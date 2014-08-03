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

p :: Prose -> Story ()
p (Prose xs) = Story $ \ cxt -> do

    -- get all the tiles and spaces
    proseTiles <- sequence
        [ case x of
            Word emph txt -> do
              w <- wordWidth cxt (Word emph txt)
              return $ Right $ tile (w,fromIntegral $ fontSize cxt + 5) $ const $ do
                font $ emphasisFont (fontSize cxt) (baseFont cxt) emph
                fillStyle (baseColor cxt)
                fillText (txt,0,fromIntegral $ fontSize cxt)    -- time will tell for this offset
            WordSpace n -> return $ Left $ n * spaceWidth cxt
        | x <- xs
        ]

{-
    liftIO $ sequence_
            [ case v of
                Left n -> print ("SP",n)
                Right t -> print ("T",tileWidth t)
            | v <- proseTiles
            ]
-}
    let
        findT (Left n:xs) ts = findS xs (ts,n)
        findT (Right t:xs) ts = findT xs (ts++[t])
        findT [] ts = [(ts,0)]

        findS (Left n:xs) (ts,w) = findS xs (ts,w + n)
        findS (Right t:xs) (ts,w) = (ts,w) : findT xs [t]
        findS [] (ts,w) = [(ts,w)]


    let glyphs2 :: [([Tile ()],Float)] = findT proseTiles []

{-
    liftIO $ sequence_
        [ print (map tileWidth ts,w)
        | (ts,w) <- glyphs2
        ]
-}
    let splits = splitLines (columnWidth cxt) [ (sum $ map tileWidth ts,w) | (ts,w) <- glyphs2 ]


--    liftIO $ print $ splits
    -- now finally laydown the tiles

    let
        write :: Bool -> [([Tile ()],Float)] -> Filler ()
        write lastLine xs = top $ pack $ mconcat $
              [ left gap | True <- [just `elem` [ JustCenter, JustRight]]] ++
              [ mconcat [ left $ tile | tile <- tiles ] <>
                (if sp == 0
                 then pure ()
                 else if just == Justified
                      then left gap
                      else left $ tile (sp,0) $ const $ return ()
                )
              | (tiles,sp) <- init xs ++ [(fst (last xs),0)]
              ] ++
              [ left gap | True <- [just `elem` [ JustLeft, JustCenter]]]
           where just = if lastLine && baseJust cxt == Justified
                        then JustLeft
                        else baseJust cxt

        loop []     [] = hbrace $ columnWidth $ cxt
        loop (n:ns) xs =
            write (null ns) (take n xs) <>
            loop ns (drop n xs)

    return $ ((),loop splits glyphs2)

------------------------------------------------------------------------

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
emphasisFont :: Int -> Text -> [Emphasis] -> Text
emphasisFont fontSize baseFont emph = Text.intercalate " " $
    [ t | e <- emph, Just t <- [f e] ] ++ [Text.pack $ show fontSize, baseFont]
  where
    f :: Emphasis -> Maybe Text
    f Italics   = return "italics"
    f Bold      = return "bold"
    f _         = fail "no match"


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

------------------------------------------------------------------------

-- This function should be memoize; it will return
-- the same answer for *every* call.
wordWidth :: MarkupContext -> Word -> Prelude Float
wordWidth cxt (Word emph txt) = Prelude $ saveRestore $ do
    font $ emphasisFont (fontSize cxt) (baseFont cxt) emph
    TextMetrics w <- measureText txt
    return w
