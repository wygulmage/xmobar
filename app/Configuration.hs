{-# LANGUAGE FlexibleContexts, CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Configuration
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Wed Nov 21, 2018 23:13
--
--
-- Parsing configuration files
--
------------------------------------------------------------------------------


module Configuration (readConfig, readDefaultConfig) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int)
import Text.ParserCombinators.Parsec.Perm ((<|?>), (<$?>), permute)
import Control.Monad.IO.Class (liftIO)

import System.Environment
import System.Posix.Files (fileExist)

import qualified Xmobar as X

#if defined XFT || defined UTF8
import qualified System.IO as S (readFile,hGetLine)
#endif

readFileSafe :: FilePath -> IO String
#if defined XFT || defined UTF8
readFileSafe = S.readFile
#else
readFileSafe = readFile
#endif

stripComments :: String -> String
stripComments =
  unlines . map (drop 5 . strip False . (replicate 5 ' '++)) . lines
    where strip m ('-':'-':xs) = if m then "--" ++ strip m xs else ""
          strip m ('"':xs) = '"': strip (not m) xs
          strip m (x:xs) = x : strip m xs
          strip _ [] = []

-- | Parse the config, logging a list of fields that were missing and replaced
-- by the default definition.
parseConfig :: String -> Either ParseError (X.Config,[String])
parseConfig = runParser parseConf fields "Config" . stripComments
    where
      parseConf = do
        many space
        sepEndSpc ["Config","{"]
        x <- perms
        eof
        s <- getState
        return (x,s)

      perms = permute $ X.Config
              <$?> pFont <|?> pFontList <|?> pWmClass <|?> pWmName
              <|?> pBgColor <|?> pFgColor
              <|?> pPosition <|?> pTextOffset <|?> pTextOffsets
              <|?> pIconOffset <|?> pBorder
              <|?> pBdColor <|?> pBdWidth <|?> pAlpha <|?> pHideOnStart
              <|?> pAllDesktops <|?> pOverrideRedirect <|?> pPickBroadest
              <|?> pLowerOnStart <|?> pPersistent <|?> pIconRoot
              <|?> pCommands <|?> pSepChar <|?> pAlignSep <|?> pTemplate


      fields    = [ "font", "additionalFonts","bgColor", "fgColor"
                  , "wmClass", "wmName", "sepChar"
                  , "alignSep" , "border", "borderColor" ,"template"
                  , "position" , "textOffset", "textOffsets", "iconOffset"
                  , "allDesktops", "overrideRedirect", "pickBroadest"
                  , "hideOnStart", "lowerOnStart", "persistent", "iconRoot"
                  , "alpha", "commands"
                  ]

      pFont = strField X.font "font"
      pFontList = strListField X.additionalFonts "additionalFonts"
      pWmClass = strField X.wmClass "wmClass"
      pWmName = strField X.wmName "wmName"
      pBgColor = strField X.bgColor "bgColor"
      pFgColor = strField X.fgColor "fgColor"
      pBdColor = strField X.borderColor "borderColor"
      pSepChar = strField X.sepChar "sepChar"
      pAlignSep = strField X.alignSep "alignSep"
      pTemplate = strField X.template "template"

      pTextOffset = readField X.textOffset "textOffset"
      pTextOffsets = readIntList X.textOffsets "textOffsets"
      pIconOffset = readField X.iconOffset "iconOffset"
      pPosition = readField X.position "position"
      pHideOnStart = readField X.hideOnStart "hideOnStart"
      pLowerOnStart = readField X.lowerOnStart "lowerOnStart"
      pPersistent = readField X.persistent "persistent"
      pBorder = readField X.border "border"
      pBdWidth = readField X.borderWidth "borderWidth"
      pAllDesktops = readField X.allDesktops "allDesktops"
      pOverrideRedirect = readField X.overrideRedirect "overrideRedirect"
      pPickBroadest = readField X.pickBroadest "pickBroadest"
      pIconRoot = readField X.iconRoot "iconRoot"
      pAlpha = readField X.alpha "alpha"

      pCommands = field X.commands "commands" readCommands

      staticPos = do string "Static"
                     wrapSkip (string "{")
                     p <- many (noneOf "}")
                     wrapSkip (string "}")
                     string ","
                     return ("Static {"  ++ p  ++ "}")
      tillFieldEnd = staticPos <|> many (noneOf ",}\n\r")

      commandsEnd  = wrapSkip (string "]") >> (string "}" <|> notNextRun)
      notNextRun = do {string ","
                      ; notFollowedBy $ wrapSkip $ string "Run"
                      ; return ","
                      }
      readCommands = manyTill anyChar (try commandsEnd) >>=
                        read' commandsErr . flip (++) "]"
      strField e n = field e n strMulti

      strMulti = scan '"'
          where
            scan lead = do
                spaces
                char lead
                s <- manyTill anyChar (rowCont <|> unescQuote)
                (char '"' >> return s) <|> fmap (s ++) (scan '\\')
            rowCont    = try $ char '\\' >> string "\n"
            unescQuote = lookAhead (noneOf "\\") >> lookAhead (string "\"")

      strListField e n = field e n strList
      strList = do
        spaces
        char '['
        list <- sepBy (strMulti >>= \x -> spaces >> return x) (char ',')
        spaces
        char ']'
        return list

      wrapSkip   x = many space >> x >>= \r -> many space >> return r
      sepEndSpc    = mapM_ (wrapSkip . try . string)
      fieldEnd     = many $ space <|> oneOf ",}"
      field  e n c = (,) (e X.defaultConfig) $
                     updateState (filter (/= n)) >> sepEndSpc [n,"="] >>
                     wrapSkip c >>= \r -> fieldEnd >> return r
      readField a n = field a n $ tillFieldEnd >>= read' n

      readIntList d n = field d n intList
      intList = do
        spaces
        char '['
        list <- sepBy (spaces >> int >>= \x-> spaces >> return x) (char ',')
        spaces
        char ']'
        return list

      read' d s = case reads s of
                    [(x, _)] -> return x
                    _ -> fail $ "error reading the " ++ d ++ " field: " ++ s

commandsErr :: String
commandsErr = "commands: this usually means that a command could not" ++
              "\nbe parsed." ++
              "\nThe error could be located at the begining of the command" ++
              "\nwhich follows the offending one."

-- | Reads the configuration files or quits with an error
readConfig :: FilePath -> String -> IO (X.Config,[String])
readConfig f usage = do
  file <- liftIO $ fileExist f
  s <- liftIO $ if file then readFileSafe f else error $
                  f ++ ": file not found!\n" ++ usage
  either (\err -> error $ f ++
                    ": configuration file contains errors at:\n" ++ show err)
         return $ parseConfig s

-- | Read default configuration file or load the default config
readDefaultConfig :: String -> IO (X.Config,[String])
readDefaultConfig usage = do
  xdgConfigFile <- X.getXdgConfigFile
  xdgConfigFileExists <- liftIO $ fileExist xdgConfigFile
  home <- liftIO $ getEnv "HOME"
  let defaultConfigFile = home ++ "/.xmobarrc"
  defaultConfigFileExists <- liftIO $ fileExist defaultConfigFile
  if xdgConfigFileExists
    then readConfig xdgConfigFile usage
    else if defaultConfigFileExists
         then readConfig defaultConfigFile usage
         else return (X.defaultConfig,[])
