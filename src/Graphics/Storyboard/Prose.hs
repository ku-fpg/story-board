{-# LANGUAGE ScopedTypeVariables, KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Prose
  ( -- * Prose is story-board's version of Text
    Prose
    -- * Prose Style Environment
  , TheProseStyle(..)
  , defaultProseStyle
    -- * Prose Builders
  , prose
  , sizedSpace
  , br
  , space
    -- * Prose Combinators
  , (<+>)
  , (</>)
    --- * Rendering
  , renderText
  , renderProse
  , fontName
    -- * Markup Style Combinators
  , ProseStyle(..)
  , i
  , b
  , font
  , fontSize
  , scaleFont
  , big
  , small
  , color
  , plain
  , wordSpacing
  , ligature
  , noLigatures
  , super
  , sub
  , boxy
  ) where

import Control.Monad (when)

-- When debugging
import           Control.Monad.IO.Class

import           Data.Char (isSpace)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.List as List
import           Data.Semigroup
import           Data.String (IsString(fromString))

import qualified Graphics.Blank as Blank
import           Graphics.Blank hiding (font)
import           Graphics.Storyboard.Literals
import           Graphics.Storyboard.Mosaic
import           Graphics.Storyboard.Prelude
import           Graphics.Storyboard.Tile
import           Graphics.Storyboard.Types

------------------------------------------------------------------------

data Prose
      = ProseItem  Text   -- can include fixed space, but will not usually.
      | ProseSpace Double  -- normalize to 1 for regular space
      | ProseScope (TheProseStyle -> TheProseStyle) Prose
      | ProseConcat [Prose]

instance IsString Prose where
  fromString txt = ProseConcat $ List.intersperse br
      [ ProseConcat $ List.intersperse space
        [ ProseItem $ Text.pack $ wd
        | wd <- parts isSpace ln
        ]
      | ln <- parts (== '\n') txt
      ]
    where

-- concat (interperse ' ' (parts isSpace xs)) == xs
parts :: (a -> Bool) -> [a] -> [[a]]
parts _ [] = []
parts p xs = case span (not . p) xs of
               (before,[_])    -> before : [[]]
               (before,(_:ys)) -> before : parts p ys
               (before,[])     -> [before]

prose :: String -> Prose
prose = fromString

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

sizedSpace :: Double -> Prose
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


data TheProseStyle = TheProseStyle
  { theFont            :: Text       -- ^ which font, "sans-serif"
  , theFontSize        :: Int        -- ^ how big, 32
  , theSpaceWidth      :: Double      -- ^ size of space, ratio of font size, 0.28 * 32
  , isItalic           :: Bool
  , isBold             :: Bool
  , subSuper           :: Int        -- 0 == regular, 1 == super, 2 == super.super, -1 = sub
  , theColor           :: Text       -- ^ current color, black
  , theLigatures       :: [(Text,Text)]
  , theDescenderHeight :: Double      -- Extra gap below baseline for descenders, ratio of font size, 0.35
  , isBoxy             :: Bool       -- ^ do you surround every word with a box
  } deriving Show

defaultProseStyle :: TheProseStyle
defaultProseStyle = TheProseStyle
  { theFont            = "sans-serif"
  , theFontSize        = 32
  , theSpaceWidth      = 0.26
  , isItalic           = False
  , isBold             = False
  , subSuper           = 0
  , theColor           = "black"
  , theLigatures       = []
  , theDescenderHeight = 0.35
  , isBoxy             = False
  }

class ProseStyle a where
  proseStyle   :: (TheProseStyle -> TheProseStyle) -> a -> a

instance ProseStyle TheProseStyle where
  proseStyle   f s = f s

instance ProseStyle Prose where
  proseStyle   f s = ProseScope f s

i           :: ProseStyle a =>          a -> a
i             = proseStyle $ \ s -> s { isItalic = True }

b             = proseStyle $ \ s -> s { isBold = True }
b           :: ProseStyle a =>          a -> a

font        :: ProseStyle a => Text ->  a -> a
font        f = proseStyle $ \ s -> s { theFont = f }

fontSize    :: ProseStyle a => Int  ->  a -> a
fontSize    n = proseStyle $ \ s -> s { theFontSize = n }

scaleFont   :: ProseStyle a => Double ->  a -> a
scaleFont  sc = proseStyle $ \ s -> s { theFontSize = rounding $ fromIntegral (theFontSize s) * sc }
  where
    rounding = if sc >= 1 then ceiling else floor

big         :: ProseStyle a =>          a -> a
big           = scaleFont (1.2)

small       :: ProseStyle a =>          a -> a
small         = scaleFont (1 / 1.2)

color       :: ProseStyle a => Text ->  a -> a
color       c = proseStyle $ \ s -> s { theColor = c }

plain       :: ProseStyle a =>          a -> a
plain         = proseStyle $ \ s -> s { isItalic = False, isBold = False }

wordSpacing :: ProseStyle a => Double -> a -> a
wordSpacing w = proseStyle $ \ s -> s { theSpaceWidth = w }

ligature    :: ProseStyle a => Text -> Text -> a -> a
ligature  f t = proseStyle $ \ s -> s { theLigatures = (f,t) : theLigatures s }

noLigatures :: ProseStyle a =>          a -> a
noLigatures   = proseStyle $ \ s -> s { theLigatures = [] }

super       :: ProseStyle a =>          a -> a
super         = proseStyle $ \ s -> s { subSuper = subSuper s + 1 }

sub         :: ProseStyle a =>          a -> a
sub           = proseStyle $ \ s -> s { subSuper = subSuper s - 1 }

boxy        :: ProseStyle a => Bool -> a -> a
boxy b'       = proseStyle $ \ s -> s { isBoxy = b' }

------------------------------------------------------------------------

-- figure out the full font from the style
fontName :: TheProseStyle -> Text
fontName cxt = Text.intercalate " " $
    [ "italics" | isItalic cxt ] ++
    [ "bold"    | isBold cxt ] ++
    [Text.pack $ show realFontSize, theFont cxt]
  where
      realFontSize :: Int
      realFontSize = round
                   $ fromIntegral (theFontSize cxt)
                   * (0.7 ^ abs (subSuper cxt) :: Double)

renderTextHeight :: TheProseStyle -> Double
renderTextHeight st = fromIntegral $ (ceiling
                    $ fromIntegral (theFontSize st)
                    * (1 + theDescenderHeight st) :: Int)

renderText :: TheProseStyle -> Text -> Prelude (Tile ())
renderText st txt = do
    let txt' = foldr (\ (f,t) -> Text.replace f t) txt (theLigatures st)
    w <- wordWidth (fontName st) txt'
    let off :: Double
        off = -0.4 * fromIntegral (subSuper st) * fromIntegral (theFontSize st)

    return . tile (w,renderTextHeight st)
           $ \ (Cavity _ _) -> do
      Blank.font $ fontName st
      fillStyle (theColor st)
      fillText (txt',0,fromIntegral (theFontSize st) + off)    -- time will tell for this offset
      when (isBoxy st) $ do
        beginPath()
        rect(0,0,w,fromIntegral (theFontSize st) + off)
        lineWidth 0.5;
        strokeStyle "black";
        stroke()

renderProse :: Alignment -> Double -> TheProseStyle -> Prose -> Prelude [Tile ()]
renderProse alignment w ps_cxt ps = do
    -- This function feels like it should be in a higher module

    proseTiles <- renderProse' ps_cxt ps

--    liftIO $ print ("proseTiles",proseTiles)
    {-
    liftIO $ sequence_
            [ case v of
                Left n -> print ("SP",n)
                Right t -> print ("T",tileWidth t)
            | v <- proseTiles
            ]
    -}
    let

        fixNL (Left n:Left m:xs)
            | alignment == left && isInfinite n
            = fixNL (Left n:Right (blank (0,renderTextHeight ps_cxt)):Left m:xs)
        fixNL (x:xs) = x : fixNL xs
        fixNL [] = []

        -- We Left for the width of spaces, and Right for tiles.
        findT (Left n:xs) ts = findS xs (ts,n)
        findT (Right t:xs) ts = findT xs (ts++[t])
        findT [] ts = [(ts,0)]

        findS (Left n:xs) (ts,w') = findS xs (ts,w' + n)
        findS (Right t:xs) (ts,w') = (ts,w') : findT xs [t]
        findS [] (ts,w') = [(ts,w')]


    let glyphs2 :: [([Tile ()],Double)] = findT (fixNL proseTiles) []

--    liftIO $ putStrLn "----------------"
--    liftIO $ sequence_
--        [ print (map tileWidth ts,w)
--        | (ts,w) <- glyphs2
--        ]

--    liftIO $ print ("glyphs2",glyphs2)

    let splits = splitLines w [ (sum $ map tileWidth ts,w') | (ts,w') <- glyphs2 ]


--    liftIO $ print $ splits

    -- now finally laydown the tiles

    let
        write :: Bool -> [([Tile ()],Double)] -> Tile ()
        write lastLine xs = pack $ mconcat $
              [ gap left | True <- [just `elem` [ center, right]]] ++
              [ mconcat [ anchor left $ tile' | tile' <- tiles ] <>
                (if sp == 0
                 then mempty
                 else if just == justified
                      then gap left
                      else anchor left $ blank (sp,0)
                )
              | (tiles,sp) <- init xs ++ [(fst (last xs),0)]
              ] ++
              [ gap left | True <- [just `elem` [ left, center]]]
           where just = if lastLine && alignment == justified
                        then left
                        else alignment

        loop []     [] = []
        loop (n:ns) xs = write (null ns) (take n xs)
                       : loop ns (drop n xs)

--    liftIO $ print $ map tileWidth $ loop splits glyphs2
    return $ loop splits glyphs2

-- Given the (min) width of a space, the width of the line,
-- and a list of word widths, how many words can we accept.
splitLine :: Double -> [(Double,Double)] -> Int
splitLine lineWidth' widths = length $ takeWhile (<= lineWidth') szs
  where
    szs = [ sz + sp + rest
          | (sz,sp,rest) <-
                zip3 (map fst widths)
                     (0 : map snd widths)
                     (0 : szs)
          ]

splitLines :: Double -> [(Double,Double)] -> [Int]
splitLines _          [] = []
splitLines lineWidth' xs = n : splitLines lineWidth' (drop n xs)
  where
    n = splitLine lineWidth' xs `max` 1 -- hfill warning here

-- internal only?
renderProse' :: TheProseStyle -> Prose -> Prelude [Either Double (Tile ())]
renderProse' _  (ProseItem txt) | Text.null txt = return []
renderProse' st (ProseItem txt) = do
    t <- renderText st txt
    return [Right t]
renderProse' st (ProseSpace n) = return [ Left $ fromIntegral (theFontSize st) * n * theSpaceWidth st ]
renderProse' st (ProseScope f ps) = renderProse' (f st) ps
renderProse' st (ProseConcat pss) = fmap concat $
    sequence [ renderProse' st ps | ps <- pss ]

------------------------------------------------------------------------
