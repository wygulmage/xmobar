{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Parsers
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parsing for template substrings
--
-----------------------------------------------------------------------------

module Xmobar.X11.Parsers (parseString, Box(..), BoxBorder(..), TextRenderInfo(..), Widget(..)) where

import Xmobar.Config.Types
import Xmobar.X11.Actions

import Control.Monad (guard, mzero)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)
import Graphics.X11.Types (Button)
import Foreign.C.Types (CInt)

data Widget = Icon String | Text String

data BoxBorder = BBTop
               | BBBottom
               | BBVBoth
               | BBLeft
               | BBRight
               | BBHBoth
               | BBFull
                 deriving ( Read, Eq )
data Box = Box BoxBorder Align Int32 CInt String deriving ( Eq )
instance Read Box where
  readsPrec _ input = do
    let b = case (words input) of
             [pos]               -> Just $ Box (read pos) C 0 1 "white"
             [pos,alg]           -> Just $ Box (read pos) (read alg) 0 1 "white"
             [pos,alg,off]       -> Just $ Box (read pos) (read alg) (read off) 1 "white"
             [pos,alg,off,wdh]   -> Just $ Box (read pos) (read alg) (read off) (read wdh) "white"
             [pos,alg,off,wdh,c] -> Just $ Box (read pos) (read alg) (read off) (read wdh) c
             _ -> Nothing
    case b of
      Just b' -> [(b', "")]
      _ -> []
data TextRenderInfo =
    TextRenderInfo { tColorsString   :: String
                   , tBgTopOffset    :: Int32
                   , tBgBottomOffset :: Int32
                   , tBoxes          :: [Box]
                   }
type FontIndex   = Int

-- | Runs the string parser
parseString :: Config -> String
               -> IO [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
parseString c s =
    case parse (stringParser ci 0 Nothing) "" s of
      Left  _ -> return [(Text $ "Could not parse string: " ++ s
                          , ci
                          , 0
                          , Nothing)]
      Right x -> return (concat x)
    where ci = TextRenderInfo (fgColor c) 0 0 []

allParsers :: TextRenderInfo
           -> FontIndex
           -> Maybe [Action]
           -> Parser [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
allParsers c f a =  textParser c f a
                <|> try (iconParser c f a)
                <|> try (rawParser c f a)
                <|> try (actionParser c f a)
                <|> try (fontParser c a)
                <|> try (boxParser c f a)
                <|> colorParser c f a

-- | Gets the string and combines the needed parsers
stringParser :: TextRenderInfo -> FontIndex -> Maybe [Action]
                -> Parser [[(Widget, TextRenderInfo, FontIndex, Maybe [Action])]]
stringParser c f a = manyTill (allParsers c f a) eof

-- | Parses a maximal string without markup.
textParser :: TextRenderInfo -> FontIndex -> Maybe [Action]
              -> Parser [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
textParser c f a = do s <- many1 $
                            noneOf "<" <|>
                              try (notFollowedBy' (char '<')
                                    (try (string "fc=")  <|>
                                     try (string "box=")  <|>
                                     try (string "fn=")  <|>
                                     try (string "action=") <|>
                                     try (string "/action>") <|>
                                     try (string "icon=") <|>
                                     try (string "raw=") <|>
                                     try (string "/fn>") <|>
                                     try (string "/box>") <|>
                                     string "/fc>"))
                      return [(Text s, c, f, a)]

-- | Parse a "raw" tag, which we use to prevent other tags from creeping in.
-- The format here is net-string-esque: a literal "<raw=" followed by a
-- string of digits (base 10) denoting the length of the raw string,
-- a literal ":" as digit-string-terminator, the raw string itself, and
-- then a literal "/>".
rawParser :: TextRenderInfo
          -> FontIndex
          -> Maybe [Action]
          -> Parser [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
rawParser c f a = do
  string "<raw="
  lenstr <- many1 digit
  char ':'
  case reads lenstr of
    [(len,[])] -> do
      guard ((len :: Integer) <= fromIntegral (maxBound :: Int))
      s <- count (fromIntegral len) anyChar
      string "/>"
      return [(Text s, c, f, a)]
    _ -> mzero

-- | Wrapper for notFollowedBy that returns the result of the first parser.
--   Also works around the issue that, at least in Parsec 3.0.0, notFollowedBy
--   accepts only parsers with return type Char.
notFollowedBy' :: Parser a -> Parser b -> Parser a
notFollowedBy' p e = do x <- p
                        notFollowedBy $ try (e >> return '*')
                        return x

iconParser :: TextRenderInfo -> FontIndex -> Maybe [Action]
              -> Parser [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
iconParser c f a = do
  string "<icon="
  i <- manyTill (noneOf ">") (try (string "/>"))
  return [(Icon i, c, f, a)]

actionParser :: TextRenderInfo -> FontIndex -> Maybe [Action]
                -> Parser [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
actionParser c f act = do
  string "<action="
  command <- choice [between (char '`') (char '`') (many1 (noneOf "`")),
                   many1 (noneOf ">")]
  buttons <- (char '>' >> return "1") <|> (space >> spaces >>
    between (string "button=") (string ">") (many1 (oneOf "12345")))
  let a = Spawn (toButtons buttons) command
      a' = case act of
        Nothing -> Just [a]
        Just act' -> Just $ a : act'
  s <- manyTill (allParsers c f a') (try $ string "</action>")
  return (concat s)

toButtons :: String -> [Button]
toButtons = map (\x -> read [x])

-- | Parsers a string wrapped in a color specification.
colorParser :: TextRenderInfo -> FontIndex -> Maybe [Action]
               -> Parser [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
colorParser (TextRenderInfo _ _ _ bs) f a = do
  c <- between (string "<fc=") (string ">") colors
  let colorParts = break (==':') c
  let (ot,ob) = case break (==',') (Prelude.drop 1 $ snd colorParts) of
             (top,',':btm) -> (top, btm)
             (top,      _) -> (top, top)
  s <- manyTill
       (allParsers (TextRenderInfo (fst colorParts) (fromMaybe (-1) $ readMaybe ot) (fromMaybe (-1) $ readMaybe ob) bs) f a)
       (try $ string "</fc>")
  return (concat s)

-- | Parses a string wrapped in a box specification.
boxParser :: TextRenderInfo -> FontIndex -> Maybe [Action]
              -> Parser [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
boxParser (TextRenderInfo cs ot ob bs) f a = do
  c <- between (string "<box=") (string ">") (many1 (alphaNum <|> char ' ' <|> char '#'))
  let b = fromMaybe (Box BBFull C 0 1 "white") $ readMaybe c
  s <- manyTill
       (allParsers (TextRenderInfo cs ot ob (b : bs)) f a)
       (try $ string "</box>")
  return (concat s)
 
-- | Parsers a string wrapped in a font specification.
fontParser :: TextRenderInfo -> Maybe [Action]
              -> Parser [(Widget, TextRenderInfo, FontIndex, Maybe [Action])]
fontParser c a = do
  f <- between (string "<fn=") (string ">") colors
  s <- manyTill (allParsers c (fromMaybe 0 $ readMaybe f) a) (try $ string "</fn>")
  return (concat s)

-- | Parses a color specification (hex or named)
colors :: Parser String
colors = many1 (alphaNum <|> char ',' <|> char ':' <|> char '#')
