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


module Xmobar.X11.Types (X, XConf (..)) where

import Graphics.X11.Xlib
import Control.Monad.Reader
import Data.Map

import Xmobar.X11.Bitmap
import Xmobar.X11.XUtil
import Xmobar.Config

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
