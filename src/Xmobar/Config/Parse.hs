{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Config.Parse
-- Copyright: (c) 2018, 2020 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Nov 25, 2018 23:56
--
--
-- Parsing of configuration files
--
------------------------------------------------------------------------------


module Xmobar.Config.Parse(readConfig, parseConfig) where

import Text.Parsec
    (Parsec, ParseError, getState, modifyState, runParser)
import Text.Parser.Char (anyChar, char, noneOf, oneOf, space, spaces, string)
import Text.Parser.Combinators (eof, manyTill, notFollowedBy, sepBy, try)
import Text.Parser.LookAhead (lookAhead)
import Text.Parser.Permutation ((<$?>), (<|?>), permute)
import Text.Parser.Token (integer)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Foldable (traverse_)
import qualified Data.Set as Set

import Xmobar.Config.Types

import qualified Data.ByteString as Bytes
import qualified Data.ByteString.UTF8 as UTF8

type XmobarParser = Parsec String (Set.Set String)

stripComments :: String -> String
stripComments =
  unlines . fmap (drop 5 . strip False . (replicate 5 ' ' <>)) . lines
    where strip m ('-':'-':xs) = if m then "--" <> strip m xs else ""
          strip m ('"':xs) = '"': strip (not m) xs
          strip m (x:xs) = x : strip m xs
          strip _ [] = []

-- | Parse the config, using the state to log a list of fields that were missing (and replaced by the default definition). (Could easily drop parsec if we didn't care about logging.)
parseConfig :: Config -> String -> Either ParseError (Config,[String])
parseConfig defaultConfig =
  runParser parseConf fields "Config" . stripComments
    where
      parseConf :: XmobarParser (Config, [String])
      parseConf =
          many space *>
          sepEndSpc ["Config","{"] *>
          liftA2 (,) (perms <* eof) (fmap Set.toList getState)

      parseConf' =
          many space *> sepEndSpc ["Config", "{"] *> perms <* eof

      perms :: XmobarParser Config
      perms = permute $ Config
              <$?> pFont <|?> pFontList <|?> pWmClass <|?> pWmName
              <|?> pBgColor <|?> pFgColor
              <|?> pPosition <|?> pTextOffset <|?> pTextOffsets
              <|?> pIconOffset <|?> pBorder
              <|?> pBdColor <|?> pBdWidth <|?> pAlpha <|?> pHideOnStart
              <|?> pAllDesktops <|?> pOverrideRedirect <|?> pPickBroadest
              <|?> pLowerOnStart <|?> pPersistent <|?> pIconRoot
              <|?> pCommands <|?> pSepChar <|?> pAlignSep <|?> pTemplate
              <|?> pVerbose

      -- fields :: [String]
      fields :: Set.Set String
      fields    = Set.fromList
                  [ "font", "additionalFonts","bgColor", "fgColor"
                  , "wmClass", "wmName", "sepChar"
                  , "alignSep" , "border", "borderColor" ,"template"
                  , "position" , "textOffset", "textOffsets", "iconOffset"
                  , "allDesktops", "overrideRedirect", "pickBroadest"
                  , "hideOnStart", "lowerOnStart", "persistent", "iconRoot"
                  , "alpha", "commands", "verbose"
                  ]

      pFont = strField font "font"
      pFontList = strListField additionalFonts "additionalFonts"
      pWmClass = strField wmClass "wmClass"
      pWmName = strField wmName "wmName"
      pBgColor = strField bgColor "bgColor"
      pFgColor = strField fgColor "fgColor"
      pBdColor = strField borderColor "borderColor"
      pSepChar = strField sepChar "sepChar"
      pAlignSep = strField alignSep "alignSep"
      pTemplate = strField template "template"

      pTextOffset = readField textOffset "textOffset"
      pTextOffsets = readIntList textOffsets "textOffsets"
      pIconOffset = readField iconOffset "iconOffset"
      pPosition = readField position "position"
      pHideOnStart = readField hideOnStart "hideOnStart"
      pLowerOnStart = readField lowerOnStart "lowerOnStart"
      pPersistent = readField persistent "persistent"
      pBorder = readField border "border"
      pBdWidth = readField borderWidth "borderWidth"
      pAllDesktops = readField allDesktops "allDesktops"
      pOverrideRedirect = readField overrideRedirect "overrideRedirect"
      pPickBroadest = readField pickBroadest "pickBroadest"
      pIconRoot = readField iconRoot "iconRoot"
      pAlpha = readField alpha "alpha"
      pVerbose = readField verbose "verbose"

      pCommands = field commands "commands" readCommands

      staticPos :: XmobarParser String
      staticPos =
          string "Static" *>
          wrapSkip (string "{") *>
          many (noneOf "}") <*
          wrapSkip (string "}") <*
          string "," <&>
          (\ p -> "Static {" <> p <> "}")

      tillFieldEnd :: XmobarParser String
      tillFieldEnd = staticPos <|> many (noneOf ",}\n\r")

      commandsEnd :: XmobarParser String
      commandsEnd  = wrapSkip (string "]") *> (string "}" <|> notNextRun)

      notNextRun :: XmobarParser String
      notNextRun = string "," *>
                   (notFollowedBy $ wrapSkip $ string "Run") *>
                   pure ","

      -- readCommands :: XmobarParser [Runnable]
      readCommands = manyTill anyChar (try commandsEnd) >>=
                        read' commandsErr . (<> "]")

      strField :: (Config -> a) -> String -> (a, XmobarParser String)
      strField e n = field e n strMulti

      strMulti :: XmobarParser String
      strMulti = scan '"'
        where
          scan lead =
              spaces *> char lead *>
              liftA2 (<>)
                  (manyTill anyChar (rowCont <|> unescQuote))
                  (pure mempty <* char '"' <|> scan '\\')

          rowCont = try $ char '\\' *> string "\n"

          unescQuote = lookAhead (noneOf "\\") *> lookAhead (string "\"")

      strListField ::
          (Config -> a) -> String -> (a, XmobarParser [String])
      strListField e n = field e n strList

      strList :: XmobarParser [String]
      strList =
          spaces *>
          char '[' *>
          sepBy (strMulti <* spaces) (char ',') <*
          spaces <*
          char ']'

      wrapSkip :: XmobarParser a -> XmobarParser a
      wrapSkip   x = many space *> x <* many space

      sepEndSpc :: [String] -> XmobarParser ()
      sepEndSpc    = traverse_ (wrapSkip . try . string)

      fieldEnd :: XmobarParser String
      fieldEnd     = many $ space <|> oneOf ",}"

      field ::
          (Config -> a) -> String -> XmobarParser b ->
          (a, XmobarParser b)
      field  e n c = (,) (e defaultConfig) $
                     modifyState (Set.delete n) *> sepEndSpc [n,"="] *>
                     wrapSkip c <* fieldEnd

      readField ::
          (Read b)=>
          (Config -> a) -> String ->
          -- (a, XmobarParser b)
          (a, Parsec String (Set.Set String) b)
      readField a n = field a n $ tillFieldEnd >>= read' n

      readIntList ::
          (Config -> a) -> String ->
          (a, XmobarParser [Int])
      readIntList d n = field d n intList

      intList :: XmobarParser [Int]
      intList =
          spaces *>
          char '[' *>
          sepBy (spaces *> fmap fromInteger integer <* spaces) (char ',') <*
          spaces <*
          char ']'

      -- read' :: (Read a, MonadFail m)=> String -> String -> m a
      read' :: (Read a)=> String -> String -> XmobarParser a
      read' d s =
          case maybeRead s of
          Just x  -> pure x
          Nothing -> fail $ "error reading the " <> d <> " field: " <> s
      maybeRead s =
          case reads s of
          [(x, _)] -> Just x
          _        -> Nothing

commandsErr :: String
commandsErr = "commands: this usually means that a command could not" <>
              "\nbe parsed." <>
              "\nThe error could be located at the begining of the command" <>
              "\nwhich follows the offending one."

-- | Reads the configuration from a file or an error if it cannot be
-- parsed.
readConfig :: Config -> FilePath -> IO (Either ParseError (Config,[String]))
readConfig defaultConfig f =
  liftIO (Bytes.readFile f <&> UTF8.toString) <&> parseConfig defaultConfig


