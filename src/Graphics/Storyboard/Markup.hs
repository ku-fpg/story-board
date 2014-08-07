{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Markup where

import qualified Data.Text as Text
import Data.Text(Text)
import Data.Semigroup
import qualified Graphics.Blank as Blank
import Graphics.Blank(Canvas,fillStyle,fillText,saveRestore,measureText,TextMetrics(..))
import Data.List as List
import Control.Applicative

import Graphics.Storyboard.Slide
import Graphics.Storyboard.Layout
import Graphics.Storyboard.Bling
import Graphics.Storyboard.Tile
import Graphics.Storyboard.Literals
import Graphics.Storyboard.Prose
import Graphics.Storyboard.Environment
import Graphics.Storyboard.Mosaic
import Graphics.Storyboard.Bling

import Control.Monad.IO.Class

------------------------------------------------------------------------

-- | build a tile around a word, but do not place it.

word :: Text -> Slide (Tile ())
word txt = slide $ \ cxt (w,h) -> do
    w <- wordWidth cxt (Word [] txt)
    return ( tile (w,fromIntegral $ theFontSize cxt + 5) $ const $ do
        Blank.font $ emphasisFont (theFontSize cxt) (theFont cxt) []
        fillStyle (theColor cxt)
        fillText (txt,0,fromIntegral $ theFontSize cxt), pure ())

------------------------------------------------------------------------

item :: Text -> Prose -> Slide ()
item txt prose = do
  cxt <- environment
  t <- word txt
  draw (pack ((blank (theLeftMargin cxt,0) ? left) <> (point top right t ? left)) ? top)
  p prose

p :: Prose -> Slide ()
p (Prose xs) = slide $ \ cxt (w,h) -> do

    -- get all the tiles and spaces
    proseTiles <- sequence
        [ case x of
            Right (Word emph txt) -> do
              let txt' = foldr (\ (f,t) -> Text.replace f t) txt (theLigatures cxt)
              w <- wordWidth cxt (Word emph txt')
              let off = if Super `elem` emph then (-5) else 0
              return $ Right $ tile (w,fromIntegral $ theFontSize cxt + 5) $ const $ do
                Blank.font $ emphasisFont (theFontSize cxt) (theFont cxt) emph
                fillStyle (theColor cxt)
                fillText (txt',0,fromIntegral $ theFontSize cxt + off)    -- time will tell for this offset
            Left n -> return $ Left $ n * theSpaceWidth cxt
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

    liftIO $ putStrLn "----------------"
    liftIO $ sequence_
        [ print (map tileWidth ts,w)
        | (ts,w) <- glyphs2
        ]

    liftIO $ print (w,[ (sum $ map tileWidth ts,w) | (ts,w) <- glyphs2 ])

    let splits = splitLines (w - (theLeftMargin cxt + theRightMargin cxt))
                            [ (sum $ map tileWidth ts,w) | (ts,w) <- glyphs2 ]


    liftIO $ print $ splits

    liftIO $ print $ cxt
    -- now finally laydown the tiles

    let
        write :: Bool -> [([Tile ()],Float)] -> Mosaic ()
        write lastLine xs = anchor top $ pack $ mconcat $
              [ gap left | True <- [just `elem` [ center, right]]] ++
              [ anchor left (tile (theLeftMargin cxt,0) $ const $ pure ())] ++
              [ mconcat [ anchor left $ tile | tile <- tiles ] <>
                (if sp == 0
                 then pure ()
                 else if just == justified
                      then gap left
                      else anchor left $ tile (sp,0) $ const $ return ()
                )
              | (tiles,sp) <- init xs ++ [(fst (last xs),0)]
              ] ++
              [ anchor left (tile (theRightMargin cxt,0) $ const $ pure ())] ++
              [ gap left | True <- [just `elem` [ left, center]]]
           where just = if lastLine && theAlignment cxt == justified
                        then left
                        else theAlignment cxt

        loop []     [] = anchor top $ blank (w,theParSkip cxt)
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
wordWidth :: Environment -> Word -> Prelude Float
wordWidth cxt (Word emph txt) = Prelude $ saveRestore $ do
    Blank.font $ emphasisFont (theFontSize cxt) (theFont cxt) emph
    TextMetrics w <- measureText txt
    return w
