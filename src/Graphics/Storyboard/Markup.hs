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
import Graphics.Storyboard.Prelude

import Control.Monad.IO.Class

------------------------------------------------------------------------

renderText :: TheProseStyle -> Text -> Prelude (Tile ())
renderText st txt = do
    let txt' = foldr (\ (f,t) -> Text.replace f t) txt (theLigatures st)
    w <- wordWidth st txt'
    let off = 0 -- if Super `elem` emph then (-5) else 0
    return $ tile (w,fromIntegral $ theFontSize st + 5) $ const $ do
      Blank.font $ fontName st
      fillStyle (theColor st)
      fillText (txt',0,fromIntegral $ theFontSize st + off)    -- time will tell for this offset

renderProse :: TheProseStyle -> Prose -> Prelude [Either Float (Tile ())]
renderProse st (ProseItem txt) = do
    t <- renderText st txt
    return [Right t]
renderProse st (ProseSpace n) = return [ Left $ n * theSpaceWidth st ]
renderProse st (ProseScope f ps) = renderProse (f st) ps
renderProse st (ProseConcat pss) = fmap concat $
    sequence [ renderProse st ps | ps <- pss ]

------------------------------------------------------------------------

-- | build a tile around a word, but do not place it.

word :: Text -> Slide (Tile ())
word txt = slide $ \ cxt (w,h) -> undefined
{-
    let ps_cxt = theProseStyle cxt
    w <- wordWidth cxt (Word [] txt)
    return ( tile (w,fromIntegral $ theFontSize ps_cxt + 5) $ const $ do
        Blank.font $ emphasisFont (theFontSize ps_cxt) (theFont ps_cxt) []
        fillStyle (theColor ps_cxt)
        fillText (txt,0,fromIntegral $ theFontSize ps_cxt), pure ())
-}
------------------------------------------------------------------------

item :: Text -> Prose -> Slide ()
item txt prose = do
  cxt <- environment
  t <- word txt
  draw (pack ((blank (theLeftMargin cxt,0) ? left) <> (point top right t ? left)) ? top)
  p prose

p :: Prose -> Slide ()
p ps = slide $ \ cxt (w,h) -> do
    let ps_cxt = theProseStyle cxt


    proseTiles <- renderProse ps_cxt ps
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
