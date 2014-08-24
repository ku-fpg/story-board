{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Highlight where

import Data.List
import Data.Function
import Data.Monoid
import Text.Regex -- regex-compat

import Debug.Trace

import Graphics.Storyboard.Prose

data TheHighlightStyle = TheHighlightStyle
  { highlightKeyword  :: TheProseStyle -> TheProseStyle
  , highlightLiteral  :: TheProseStyle -> TheProseStyle
  , highlightString   :: TheProseStyle -> TheProseStyle
  , highlightConstant :: TheProseStyle -> TheProseStyle
  , highlightComment  :: TheProseStyle -> TheProseStyle
  , theREs :: [(Regex,TheHighlightStyle -> String -> String -> Prose)]
  , theNextHighlight :: Maybe TheHighlightStyle
  }

defaultHighlightStyle = TheHighlightStyle
  { highlightKeyword  = b
  , highlightLiteral  = color "blue" . b
  , highlightString   = color "green"
  , highlightConstant = color "red"
  , highlightComment  = color "#723282"
  , theREs = matches [("(\n|.)+",\ st str rest -> prose str <> highlight st rest)]
  , theNextHighlight  = Nothing
  }


instance Show TheHighlightStyle where
  show _ = "TheHighlightStyle{}"

accept :: (TheHighlightStyle -> TheProseStyle -> TheProseStyle) -> TheHighlightStyle -> String -> String -> Prose
accept f st this next = proseStyle (f st) (prose this) <> highlight st next

matches :: [(String,TheHighlightStyle -> String -> String -> Prose)]
        -> [(Regex,TheHighlightStyle -> String -> String -> Prose)]
matches res = [ (mkRegex ('^' : s),f) | (s,f) <- res ]

pop :: TheHighlightStyle -> TheHighlightStyle
pop st = case theNextHighlight st of
           Nothing -> error "pop error"
           Just st' -> st'

highlightLess :: TheHighlightStyle -> TheProseStyle -> TheProseStyle
highlightLess = const id

--(~=) :: String -> (String -> String -> Prose) -> (String,String -> String -> Prose)

ghciHighlightStyle :: TheHighlightStyle
ghciHighlightStyle = defaultHighlightStyle
 { theREs = matches
      [ ("Prelude>",
          \ st this rest ->
              proseStyle (highlightKeyword st) (prose this) <>
              highlight haskellHighlightStyle (takeWhile (/= '\n') rest) <>
              highlight st (dropWhile (/= '\n') rest))
      , ("(.|\n)",\ st str rest -> prose str <> highlight st rest)
      ]
 , highlightKeyword  = b . color "red"
 }

haskellHighlightStyle :: TheHighlightStyle
haskellHighlightStyle = defaultHighlightStyle
 { theREs = matches $
    [("[ \n\t]+",accept (const id))] ++
    startComment ++
    [ (kw,accept highlightKeyword)
    | kw <- words $
        "let in if then else case of where do module import hiding " ++
        "qualified type data newtype deriving class instance as default " ++
        "infix infixl infixr foreign export ccall stdcall cplusplus " ++
        "jvm dotnet safe unsafe family forall mdo proc rec"
    ] ++
    [ ("[a-z][\\.a-zA-Z0-9_']*",accept highlightLess)
    , ("[A-z][\\.a-zA-Z0-9_']*",accept highlightConstant)

    , ("::",accept highlightKeyword)
    , ("=>",accept highlightKeyword)
    , ("->",accept highlightKeyword)
    , ("=",accept highlightKeyword)
    , (",",accept highlightLess)


   , ("[0-9]+",accept highlightLiteral)
   , ("[0-9]+\\.[0-9]+",accept highlightLiteral)


   , ("\"", \ st this rest -> proseStyle (highlightString st) (prose this) <>
                let str_st = st
                      { theREs = matches [("\\\\\"",accept highlightString)
                                         ,("\"",\ st this rest ->
                                               proseStyle (highlightString st) (prose this) <>
                                               highlight (pop st) rest)
                                         ,("[^\\\"]+",accept highlightString)
                                         ,("\\\\",accept highlightString)
                                         ]
                      , theNextHighlight = Just st
                      }
                in highlight str_st rest)

    , (":[_$#<>\\+\\*\\-]",accept highlightConstant)
    , ("[_$#<>\\+\\*\\-]+",accept highlightLess)
    , ("[\\(\\)]+",accept highlightLess)
    , ("[{}\\[\\]]",accept highlightLess)
    ]
 } where
    comment st = st
      { theREs = matches (startComment ++ stopComment ++ insideComment)
      , theNextHighlight = Just st
      }
    startComment  = [("{-", \ st this rest ->
                            proseStyle (highlightComment st) (prose this) <>
                            highlight (comment st) rest
                      )]
    stopComment   = [("-}",\ st this rest ->
                            proseStyle (highlightComment st) (prose this) <>
                            highlight (pop st) rest
                      )]
    insideComment = [("(\n|[^\\-])+",accept highlightComment)
                    ,("-",accept highlightComment)
                    ]



{-
  [("of",highlight bold)]
  [("[a-z]",Accept)]
-}

highlight :: TheHighlightStyle -> String -> Prose
highlight _st "" = mempty
highlight st txt = case m of
                   [] -> case theNextHighlight st of
                          Nothing -> error $ "no match for: " ++ take 30 txt
                          Just st' -> highlight st' txt
                   (p:_) -> p
  where
    m   = map snd
        $ sortBy (flip compare `on` fst)
        $ [ -- traceShow (length d,d,a) $
              (length d,f st d a)
          | (re,f) <- theREs st
          , Just ("",d,a,_) <- return $ matchRegexAll re txt
          , not (null d)
          ]
