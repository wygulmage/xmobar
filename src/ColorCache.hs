{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- |
-- Module: ColorCache
-- Copyright: (c) 2012 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Mon Sep 10, 2012 00:27
--
--
-- Caching X colors
--
------------------------------------------------------------------------------

module ColorCache(withColors) where

#if defined XFT
-- import Graphics.X11.Xft
#endif
import Data.IORef
import Graphics.X11.Xlib
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Exception (SomeException, handle)

data DynPixel = DynPixel { allocated :: Bool
                         , pixel     :: Pixel
                         }

-- | Get the Pixel value for a named color: if an invalid name is
-- given the black pixel will be returned.
initColor :: Display -> String -> IO DynPixel
initColor dpy c = handle black $ (initColor' dpy c)
  where
    black :: SomeException -> IO DynPixel
    black = (const . return $ DynPixel False (blackPixel dpy $ defaultScreen dpy))

type ColorCache = [(String, Color)]
{-# NOINLINE colorCache #-}
colorCache :: IORef ColorCache
colorCache = unsafePerformIO $ newIORef []

getCachedColor :: String -> IO (Maybe Color)
getCachedColor color_name = lookup color_name `fmap` readIORef colorCache

putCachedColor :: String -> Color -> IO ()
putCachedColor name c_id = modifyIORef colorCache $ \c -> (name, c_id) : c

initColor' :: Display -> String -> IO DynPixel
initColor' dpy c = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  cached_color <- getCachedColor c
  c' <- case cached_color of
          Just col -> return col
          _        -> do (c'', _) <- allocNamedColor dpy colormap c
                         putCachedColor c c''
                         return c''
  return $ DynPixel True (color_pixel c')

withColors :: MonadIO m => Display -> [String] -> ([Pixel] -> m a) -> m a
withColors d cs f = do
  ps <- mapM (liftIO . initColor d) cs
  f $ map pixel ps
