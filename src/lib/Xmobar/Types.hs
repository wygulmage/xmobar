------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Types
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 19:02
--
--
-- The Xmobar basic type
--
------------------------------------------------------------------------------


module Xmobar.Types (X , XConf (..), runX) where

import Graphics.X11.Xlib
import Control.Monad.Reader
import Data.Map

import Xmobar.Config
import Xmobar.Bitmap
import Xmobar.XUtil


-- The Xmobar data type and basic loops and functions.

-- | The X type is a ReaderT
type X = ReaderT XConf IO

-- | The ReaderT inner component
data XConf =
    XConf { display   :: Display
          , rect      :: Rectangle
          , window    :: Window
          , fontListS :: [XFont]
          , verticalOffsets :: [Int]
          , iconS     :: Map FilePath Bitmap
          , config    :: Config
          }

-- | Runs the ReaderT
runX :: XConf -> X () -> IO ()
runX xc f = runReaderT f xc
