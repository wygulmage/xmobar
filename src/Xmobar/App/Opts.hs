------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.Opts
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Fri Nov 30, 2018 01:19
--
--
-- Command line option parsing
--
------------------------------------------------------------------------------


module Xmobar.App.Opts where

import System.Console.GetOpt
import Data.Version (showVersion)

import Paths_xmobar (version)

data Opts = Help
          | Debug
          | Version
          | Font       String
          | BgColor    String
          | FgColor    String
          | Alpha      String
          | T
          | B
          | D
          | AlignSep   String
          | Commands   String
          | AddCommand String
          | SepChar    String
          | Template   String
          | OnScr      String
          | IconRoot   String
          | Position   String
          | WmClass    String
          | WmName     String
       deriving (Show, Eq)

options :: [OptDescr Opts]
options =
    [ Option "h?" ["help"] (NoArg Help) "This help"
    , Option "D" ["debug"] (NoArg Debug) "Emit verbose debugging messages"
    , Option "V" ["version"] (NoArg Version) "Show version information"
    , Option "f" ["font"] (ReqArg Font "font name") "Font name"
    , Option "w" ["wmclass"] (ReqArg WmClass "class") "X11 WM_CLASS property"
    , Option "n" ["wmname"] (ReqArg WmName "name") "X11 WM_NAME property"
    , Option "B" ["bgcolor"] (ReqArg BgColor "bg color" )
      "The background color. Default black"
    , Option "F" ["fgcolor"] (ReqArg FgColor "fg color")
      "The foreground color. Default grey"
    , Option "i" ["iconroot"] (ReqArg IconRoot "path")
      "Root directory for icon pattern paths. Default '.'"
    , Option "A" ["alpha"] (ReqArg Alpha "alpha")
      "Transparency: 0 is transparent, 255 is opaque. Default: 255"
    , Option "o" ["top"] (NoArg T) "Place xmobar at the top of the screen"
    , Option "b" ["bottom"] (NoArg B)
      "Place xmobar at the bottom of the screen"
    , Option "d" ["dock"] (NoArg D)
      "Don't override redirect from WM and function as a dock"
    , Option "a" ["alignsep"] (ReqArg AlignSep "alignsep")
      "Separators for left, center and right text\nalignment. Default: '}{'"
    , Option "s" ["sepchar"] (ReqArg SepChar "char")
      ("Character used to separate commands in" ++
       "\nthe output template. Default '%'")
    , Option "t" ["template"] (ReqArg Template "template")
      "Output template"
    , Option "c" ["commands"] (ReqArg Commands "commands")
      "List of commands to be executed"
    , Option "C" ["add-command"] (ReqArg AddCommand "command")
      "Add to the list of commands to be executed"
    , Option "x" ["screen"] (ReqArg OnScr "screen")
      "On which X screen number to start"
    , Option "p" ["position"] (ReqArg Position "position")
      "Specify position of xmobar. Same syntax as in config file"
    ]

getOpts :: [String] -> IO ([Opts], [String])
getOpts argv =
    case getOpt Permute options argv of
      (o,n,[])   -> return (o,n)
      (_,_,errs) -> error (concat errs ++ usage)

usage :: String
usage = usageInfo header options ++ footer
    where header = "Usage: xmobar [OPTION...] [FILE]\nOptions:"
          footer = "\nMail bug reports and suggestions to " ++ mail ++ "\n"

info :: String
info = "xmobar " ++ showVersion version
        ++ "\n (C) 2007 - 2010 Andrea Rossato "
        ++ "\n (C) 2010 - 2018 Jose A Ortega Ruiz\n "
        ++ mail ++ "\n" ++ license

mail :: String
mail = "<mail@jao.io>"

license :: String
license = "\nThis program is distributed in the hope that it will be useful," ++
          "\nbut WITHOUT ANY WARRANTY; without even the implied warranty of" ++
          "\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE." ++
          "\nSee the License for more details."
