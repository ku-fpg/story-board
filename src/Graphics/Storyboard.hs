{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs, StandaloneDeriving, TypeFamilies, DataKinds #-}
module Graphics.Storyboard where

import Graphics.Blank hiding (eval)
import Data.Semigroup
import Control.Applicative
import Control.Monad
import Data.Text(Text)
import qualified Data.Text as Text
import Data.String
import Data.List
import Data.Maybe

-----------------------------------------------------------------------------

data EMState = EMState Int [(E,Op,E)] deriving Show

data Op = LessEq deriving (Eq,Ord,Show)

newtype EM a = EM { runEM :: EMState -> (a,EMState) }

instance Functor EM where
 fmap f m = pure f <*> m

instance Applicative EM where
 pure    = return
 f <*> m = liftM2 ($) f m

instance Monad EM where
  return a = EM $ \ st -> (a,st)
  EM f >>= k = EM $ \ st ->
    case f st of
       (a,st') -> runEM (k a) st'


newtype U = U Int deriving Show

newE :: EM U
newE = EM $ \ st@(EMState u ues) -> (U u,EMState (u+1) ues)


--assign :: U -> E -> EM ()
--assign v e = EM $ \ (EMState u ues) -> ((),EMState u $ (v,e):ues)

le :: E -> E -> EM ()
le e1 e2 = EM $ \ (EMState u ues) -> ((),EMState u $ (e1,LessEq,e2):ues)


prettyEq :: (E,Op,E) -> String
prettyEq (e1,op,e2) = prettyE e1 ++ showOp ++ prettyE e2
   where
       showOp = case op of {LessEq -> "<=" }


-----------------------------------------------------------------------------

type Size  f = (f,f)
type Coord f = (f,f)

-----------------------------------------------------------------------------

-- A Tile has a specific, fixed size.
-- When rendered, it is given a specific size to operate inside of,
-- that typically would be *at least* the size of the original fixed size.
-- The tile can choose to put any extra space on the inside or outside
-- of any border, etc.

data Tile a = Tile (Size Float) (Size Float -> Canvas a)

-----------------------------------------------------------------------------

-- A Parcel can be stretched to different sizes.
-- You give a size to fit into, and return the actual size.
--data Parcel a = Parcel { runParcel :: (Size Float) -> Canvas (a,Size Float) }

-----------------------------------------------------------------------------

data Cavity f = Cavity
  { cavityCorner :: Coord f
  , cavitySize   :: Size f
  }
  deriving Show

-- A Fill is a computation that fills a cavity.
newtype Filling a = Filling
  { runFilling :: Cavity E -> (Cavity E,Cavity Float -> Canvas (a,Cavity Float))
  }

instance Functor Filling where
 fmap f m = pure f <*> m

instance Applicative Filling where
 pure a = Filling $ \ sz -> (sz,\ sz0 -> return (a,sz0))
 Filling f <*> Filling x = Filling $ \ sz ->
    let
      (sz',kf) = f sz
      (sz'',kx) = x sz'
    in
      (sz'',\ sz0 -> do
                    (f',sz1) <- kf sz0
                    (x',sz2) <- kx sz1
                    return (f' x',sz2))

{-
instance Monad Filling where
  -- A state monad, where the state is the size of the cavity
  return a = Filling $ \ sz -> return (return a,sz)
  Filling f >>= k = Filling $ \ sz -> do
-}

-----------------------------------------------------------------------------
{-
tile :: (Size -> Cavity -> Cavity) -> (Size -> Cavity -> Coord) -> Tile a -> Filling a
tile f g (Tile (w,h) m) = Filling $ \ cavity -> do
  a <- saveRestore $ do
    translate (g (w,h) cavity)
    m
  return (a,f (w,h) cavity)
-}

tileTop :: Tile a -> Filling a
tileTop (Tile (w,h) k) = Filling $ \ (Cavity (cx,cy) (cw,ch)) ->
  ( Cavity (cx,cy `add` Lit h) (Mx cw (Lit w),ch `sub` Lit h)
  , \ (Cavity (cx',cy') (cw',ch')) -> do
        a <- k (cw',h)
        return (a,Cavity (cx',cy' + h) (cw',ch' - h))
  )

tileLeft :: Tile a -> Filling a
tileLeft (Tile (w,h) k) = Filling $ \ (Cavity (cx,cy) (cw,ch)) ->
  ( Cavity (cx `add` Lit w,cy) (cw `sub` Lit w,ch `Mx` Lit h)
  , \ (Cavity (cx',cy') (cw',ch')) -> do
        a <- k (w,ch')
        return (a,Cavity (cx' + w,cy') (cw' - w,ch'))
  )



{-
  let (Tile (w,h) m) = theTile
  w     `le` cw
  Lit 0 `le` (ch `sub` h)  -- this can move to a "Filling" wrapper
  return ( saveRestore $ do
             translate (eval cx,eval cy)
             m (w,h)
         )
-}

--fillingSize :: Filling (Size Float)
--fillingSize = Filling $ \ cavity@(Cavity _ sz) -> return (sz,cavity)

--wrap :: Filling a -> Tile a
--wrap (Filling filler) = Tile

--fillTop :: Float -> Filling a -> Filling a
--fillTop h m = do
--    (w,_) <- fillingSize
--    tileTop $ fillTile (w,h) $ m

-- turn a Filling into a tile.

fillTile :: Filling a -> Tile a
fillTile filling = Tile (w,h) $ \ (w',h') -> fst <$> k (Cavity (0,0) (w',h'))
  where
    (w,h)      = (100,100)
    (cavity,k) = runFilling filling (Cavity (Lit 0,Lit 0) (Width,Height))

--    return (undefined,undefined)
{-
    (a,Cavity (x,y) (w,h)) <- filler (Cavity (Lit 0,Lit 0) (Lit x',Lit y'))-- (Var "x",Var "y"))
    saveRestore $ do
      beginPath()
      fillStyle "#abcdef"
      rect(eval x,eval y,eval w,eval h)
      closePath()
      fill()
      return a
-}
{-
fillTile :: Filling a -> Parcel a
fillTile sz (Filling filler) = Tile sz $ do
    (a,Cavity (x,y) (w,h)) <- filler (Cavity (0,0) sz)
    saveRestore $ do
      beginPath()
      fillStyle "#abcdef"
      rect(x,y,w,h)
      closePath()
      fill()
      return a
-}

-----------------------------------------------------------------------------

data E
    = Lit Float
    | Width
    | Height
    | Add E E
    | Sub E E
    | Mx E E
  deriving (Eq,Ord)

instance Show E where show = prettyE

prettyE :: E -> String
prettyE (Lit f) = show f
prettyE (Width)  = "w"
prettyE (Height) = "h"
prettyE (Add e1 e2) = "(" ++ prettyE e1 ++ "+" ++ prettyE e2 ++ ")"
prettyE (Sub e1 e2) = "(" ++ prettyE e1 ++ "-" ++ prettyE e2 ++ ")"
prettyE (Mx e1 e2) = "(" ++ prettyE e1 ++ "`max`" ++ prettyE e2 ++ ")"

add (Lit i) (Lit j) = Lit (i + j)
add (Add e1 e2) e3  = add e1 (add e2 e3)
add a b = Add a b
sub (Lit i) (Lit j) = Lit (i - j)
sub (Sub e1 e2) e3  = sub e1 (add e2 e3)
sub a b = Sub a b

eval :: E -> Float
eval (Lit f) = f
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

--lit <= u - lit

-----------------------------------------------------------------------------

example1 :: Text -> Tile ()
example1 col = Tile (100,100) $ \ sz -> do
        -- assumes 100 x 100 pixels sized viewport
        beginPath()
        fillStyle col
        arc(50, 50, 40, 0, pi*2, False)
        closePath()
        fill()

example2 :: Filling ()
example2 =
  (tileTop $ example1 "red")    *>
  (tileTop $ example1 "green")  *>
  (tileLeft $ example1 "red")    *>
  (tileLeft$ example1 "green")  *>
  (tileTop $ example1 "orange")


main = do
    print cavity
--    putStrLn $ unlines $ map prettyEq $ nub eqs
  where
    (cavity,_) = (runFilling example2 (Cavity (Lit 0,Lit 0) (Width,Height) ))

--          Tile _ m = fillTile (width context - 200,height context - 200) $ example2

-----------------------------------------------------------------------------
{-

-- A Box is just a function from size to canvas
type Box a = Size -> Canvas a

-- A Sized is given a size to work with
data Sized a = Sized (Size -> Canvas a)

data Resized a = Resized { runResized :: Canvas (a,Size) }

--fit :: Tile a -> Sized a -> Tile a

instance Monoid a => Monoid (Tile a) where
  mempty = Tile (0,0) (return mempty)
  mappend (Tile (x1,y1) c1) (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $
          do r1 <- c1
             r2 <- c2   -- overlay is the default monoid
             return (r1 `mappend` r2)


instance Monoid a => Monoid (Sized a) where
  mempty = Sized $ \ _ -> return mempty
  mappend (Sized f1) (Sized f2) = Sized $ \ (x,y) ->
          do r1 <- f1 (x,y)
             r2 <- f2 (x,y) -- overlay is the default monoid
             return (r1 `mappend` r2)


{-
-- Draw a compound Tile as a large picutre
drawTile :: Tile () -> Canvas ()
drawTile (Tile (w',h') picture) = do
        (w,h) <- size
        let w1 = w / w'
        let h1 = h / h'
        let s = min w1 h1
        save()
        translate(w/2,h/2)              -- center of the canvas
        translate(-s*w'/2,-s*h'/2)
        scale (s,s)
        picture
        restore()
-}

roundedBox :: Float -> Sized ()
roundedBox radius = Sized $ \ (width,height) -> saveRestore $ do
  beginPath()
  moveTo(radius, 0)
  lineTo(width - radius, 0)
  quadraticCurveTo(width, 0, width, radius)
  lineTo(width, height - radius)
  quadraticCurveTo(width, height, width - radius, height)
  lineTo(radius, height)
  quadraticCurveTo(0, height, 0, height - radius)
  lineTo(0, radius)
  quadraticCurveTo(0, 0, radius, 0)
  closePath()
  fill()

toTile :: Size -> Sized a -> Tile a
toTile sz (Sized f) = Tile sz (f sz)

-- Should this scale to fit?
toSized :: Tile a -> Sized a
toSized (Tile sz m) = Sized $ \ sz -> m

----------------------------------------------
-- Try write some combinators

-- border around things
frame :: Size -> (Size -> Tile a) -> Tile a
frame = undefined

data Pad = Pad { vertical :: VPad, horizontal :: HPad }
data VPad = T | M | B
data HPad = L | C | R

pad :: Pad -> (Size -> Tile a) -> Size -> Tile a
pad = undefined

-- Based on tk/tcl
-- The idea is that the Tile is placed on a side, shrinking the
-- cavity left for more stuff.
{-
data Side = Top

--packer :: Side -> Tile a -> Cavity a -> Cavity a


beside :: Widget () -> Widget () -> Widget ()
beside w1 w2 = Widget (sum xs,maximum (0:ys)) $ Beside w1 w2
  where
    (xs,ys) = unzip [ (x,y) | Widget (x,y) _ <- [w1,w2] ]

struct :: (Float,Float) -> Widget ()  -- small, fixed size tile
struct (w,h) = Widget (w,h) $ Struct (w,h)

spring :: Widget ()  -- spring in both dimensions
spring = Widget (0,0) $ Spring

data Widget a = Widget
      { w_size   :: (Float,Float)
      , w_inside :: InternalWidget a
      }
    deriving Show

data InternalWidget :: * -> * where
  Spring     ::                     InternalWidget ()
  Struct     :: (Float,Float)    -> InternalWidget ()
  Beside     :: Widget () -> Widget () -> InternalWidget ()

deriving instance Show (InternalWidget a)

--instance Semigroup (Widget a) where



{-
(<~>) :: Widget a -> Widget a -> Widget a  -- a <> vspring <> b
(<>)  :: Widget a -> Widget a -> Widget a   -- beside
(/~/) :: Widget a -> Widget a -> Widget a  -- a // hspring // b
(//)  :: Widget a -> Widget a -> Widget a   -- above
-}

--example1 = struct (10,10) `beside` spring `beside` struct (10,10)
{-
resizeable :: Widget a -> (Bool,Bool)
resizeable (Widget _ Spring)      = (True,True)
resizeable (Widget _ (Struct _))  = (False,False)
resizeable (Widget _ (Beside xs)) = (or ws,and ws)
    where (ws,hs) = unzip $ map resizeable xs
-}

resizeCost :: Widget a -> (Maybe Float,Maybe Float)
resizeCost (Widget _ Spring)      = (return 1,return 1)
resizeCost (Widget _ (Struct _))  = (Nothing,Nothing)
resizeCost (Widget _ (Beside w1 w2)) = (hsprings,vsprings)
    where
          (ws,hs) = unzip $ map resizeCost [w1,w2]
          -- you add the costs, because you will distribute them
          -- Nothing (no resizability) counts as 0
          hsprings = if all isJust ws
                     then Nothing
                     else Just $ sum [ x | Just x <- ws ]
          -- you add the costs, because you will distribute them
          -- Nothing (no resizability) counts as
          vsprings = if any isJust hs
                     then Nothing
                     else Just $ sum [ x | Just x <- hs ] / fromIntegral (length hs)

-- Request a specific (larger) size. Only vaild if resizeable w == (True,True)

{-
resize :: (Float,Float) -> Widget a -> Widget a
resize (w,h) widget = case resizeCost widget of
                        (Just w)
-}
-}
{-
 Concept:
   above :: Widget a -> Widget a -> Widget a
   beside :: Widget a -> Widget a -> Widget a
   spring :: Widget a
   struct :: (Float,Float) -> Widget a
   tile   :: (Float,Float) ->
-}

data Widget a b = Widget
      { w_size   :: (Float,Float)
      , w_inside :: InternalWidget a b
      }
    deriving Show

data InternalWidget :: S -> S -> * where
  Spring     ::                       InternalWidget Flex Flex
  Hstruct    :: Float              -> InternalWidget Fixed Flex
  Beside     :: Widget a1 b1
             -> Widget a2 b2       -> InternalWidget (Flexable a1 a2) (Fixable b1 b2)
  Prim       :: (Float,Float)      -> InternalWidget Fixed Fixed

deriving instance Show (InternalWidget a b)

data S = Fixed | Flex

spring :: Widget Flex Flex
spring = Widget (0,0) $ Spring

hstruct :: Float -> Widget Fixed Flex
hstruct w = Widget (w,0) $ Hstruct w

widget :: (Float,Float) -> Widget Fixed Fixed
widget (w,h) = Widget (w,h) $ Prim (w,h)

type family Flexable     (a :: S) (b :: S) :: S
type instance Flexable Flex a = Flex
type instance Flexable a Flex = Flex
type instance Flexable Fixed Fixed = Fixed

type family Fixable     (a :: S) (b :: S) :: S
type instance Fixable a Fixed = Fixed
type instance Fixable Fixed a = Fixed
type instance Fixable Flex Flex = Flex

beside :: forall a1 a2 b1 b2 . Widget a1 b1 -> Widget a2 b2 -> Widget (Flexable a1 a2) (Fixable b1 b2)
beside x1@(Widget (w1,h1) i1) x2@(Widget (w2,h2) i2)
    | h1 == h2 = Widget (w1 + w2,h1) $ Beside x1 x2
--    | h1 <  h2 = Widget (w1 + w2,h2) $ Beside (expandV h2 x1) x2
--    | h1 >  h2 = Widget (w1 + w2,h1) $ Beside x1 (expandV h1 x2)

--autoTop :: Widget a b -> Widget a Flex
--autoTop = undefined

{-
expandV :: Float -> Widget a b -> Widget a b
expandV h' (Widget (w,h) Spring)      = Widget (w,h') Spring
expandV h' (Widget (w,h) (Hstruct n)) = Widget (w,h') (Hstruct n)
expandV h' w@(Widget _ (Prim n))      = expandV' h' w spring

expandV' :: Float -> Widget a1 b1 -> Widget a2 b2 -> Widget a Flex
expandV' = undefined

--expandV h' (Widget (w,h) (Beside x1 x2)) =
--  where
--    expandV'
-}
-- How about a frame with a title?

type Pos = () -- will be top right, etc.

--place :: Tile () -> () -> Sized ()
--place (Tile (x,y) m) () = Sized $ \ (sx,sy) -> m

-- Fit this text into this width
-- Assume 12 pt font?

--fit :: Float -> Markup -> Tile ()
--fit = undefined

-- draw a line, return what was not written
drawTextLine :: Float -> Markup -> Resized Markup
drawTextLine _ (Markup ms) = Resized $ return (Markup ms,(0,0))

{-
drawLeaf :: Markup -> Resized ()
drawLeaf (Text txt) = do
        TextMetrics w <- measureText txt
        return w
-}
markupWidth :: Markup -> Canvas Float
markupWidth (Markup ms) = do
        -- assumes no space between markups.
        ws <- sequence [markedWidth m | m <- ms]
        return $ sum ws

markedWidth :: Marked -> Canvas Float
markedWidth (Node _ m) = markupWidth m
markedWidth (Leaf x)   = markupLeafWidth x

markupLeafWidth :: MarkupLeaf -> Canvas Float
markupLeafWidth (Text txt) = do
        TextMetrics w <- measureText txt
        return w
markupLeafWidth (Space) = markupLeafWidth (Text " ")

{-
splitMarkup :: Markup -> [(Markup,Markup)]
splitMarkup (Markup ms) = splitMarkups ms


splitMarkups' :: [Markup] -> Markup -> [(Markup,Markup)]
splitMarkups' [Space] before = [(before,Concat [])]
splitMarkups' [] before = [(before,Concat [])]
-}

splitMarkup :: Markup -> [(Markup,Markup)]
splitMarkup (Markup ms) = [ (Markup bs, Markup as) | (bs,as) <- splitMarkeds ms ]

splitMarkeds :: [Marked] -> [([Marked],[Marked])]
splitMarkeds ms =
                [ (take n ms,drop (n + 1) ms) | (n,Leaf Space) <- [0..] `zip` ms ] ++
                [ (ms,[])]


-- merge spaces and text into larger texts, at one level only
joinMarkeds :: [Marked] -> [Marked]
joinMarkeds = joinText . fmap (\ x -> case x of { Leaf Space -> Leaf (Text " ") ; other -> other })
  where
     joinText (Leaf (Text ns):Leaf (Text ms):rest) = joinText (Leaf (Text (ns <> ms)) : rest)
     joinText (r:rs) = r : joinText rs
     joinText [] = []




-- Markup is a sequest of marked trees. Think HTML document.
newtype Markup = Markup { unMarkup :: [Marked] }
                          deriving (Eq, Ord, Show)


instance Monoid Markup where
    mempty = Markup []
    mappend (Markup m1) (Markup m2) = Markup (m1 `mappend` m2)

-- A marked is a single tree with a common attribute. Think XML element
data Marked = Node MarkupTag Markup
            | Leaf MarkupLeaf
                          deriving (Eq, Ord, Show)

data MarkupTag = DIV -- just group things
                          deriving (Eq, Ord, Show)

data MarkupLeaf = Space             -- the only place to break
                | Text Text
                          deriving (Eq, Ord, Show)

                          {-
            | Gap Float         -- gap of this width, non breakable?
            | Font Float Text Markup
            | Italics Markup
            | Bold Markup
            | Code Markup
            | Color Text Markup
-}


example_text :: Markup
example_text = "In the process we've discovered that concatenate vanishes and tupling " `mappend`
               "transformation can be expressed as instances of worker/wrapper."


instance IsString Markup where
   fromString = Markup . intersperse (Leaf Space) . fmap (Leaf . Text . Text.pack) . words
-}

main2 = blankCanvas 3000 $ \ context -> do
      send context $ do
        fillStyle "orange"
        translate (100,100)
--        let Tile _ m = fillTile (width context - 200,height context - 200) $ example2
--        _ <- m
        return ()
{-
        case roundedBox 50 of
          Sized f -> f (width context, height context)
        (_,sz) <- runResized $ drawTextLine 200 example_text
        return ()
-}
