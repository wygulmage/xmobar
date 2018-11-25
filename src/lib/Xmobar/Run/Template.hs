------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Template
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Nov 25, 2018 05:49
--
--
-- Handling the top-level output template
--
------------------------------------------------------------------------------


module Xmobar.Run.Template(parseCommands) where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

import Xmobar.Run.Commands
import Xmobar.Run.Runnable
import Xmobar.Config

-- | Parses the output template string
templateStringParser :: Config -> Parser (String,String,String)
templateStringParser c = do
  s   <- allTillSep c
  com <- templateCommandParser c
  ss  <- allTillSep c
  return (com, s, ss)

-- | Parses the command part of the template string
templateCommandParser :: Config -> Parser String
templateCommandParser c =
  let chr = char . head . sepChar
  in  between (chr c) (chr c) (allTillSep c)

-- | Combines the template parsers
templateParser :: Config -> Parser [(String,String,String)]
templateParser = many . templateStringParser

-- | Actually runs the template parsers
parseCommands :: Config -> String -> IO [(Runnable,String,String)]
parseCommands c s =
    do str <- case parse (templateParser c) "" s of
                Left _  -> return [("", s, "")]
                Right x -> return x
       let cl = map alias (commands c)
           m  = Map.fromList $ zip cl (commands c)
       return $ combine c m str

-- | Given a finite "Map" and a parsed template produce the resulting
-- output string.
combine :: Config -> Map.Map String Runnable
           -> [(String, String, String)] -> [(Runnable,String,String)]
combine _ _ [] = []
combine c m ((ts,s,ss):xs) = (com, s, ss) : combine c m xs
    where com  = Map.findWithDefault dflt ts m
          dflt = Run $ Com ts [] [] 10

allTillSep :: Config -> Parser String
allTillSep = many . noneOf . sepChar
