{-# LANGUAGE ForeignFunctionInterface #-}
------------------------------------------------------------------------------
-- |
-- Module: MinXft
-- Copyright: (c) 2012 Jose Antonio Ortega Ruiz
--            (c) Clemens Fruhwirth <clemens@endorphin.org> 2007
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Mon Sep 10, 2012 18:12
--
--
-- Pared down Xft library, based on Graphics.X11.Xft and providing
-- explicit management of XftColors, so that they can be cached.
--
-- Most of the code is lifted from Clemens's.
--
------------------------------------------------------------------------------

module MinXft ( AXftColor
              , AXftDraw (..)
              , AXftFont
              , mallocAXftColor
              , freeAXftColor
              , withAXftDraw
              , drawXftString
              , drawXftString'
              , drawXftRect
              , openAXftFont
              , closeAXftFont
              , xftTxtExtents
              , xftTxtExtents'
              , xft_ascent
              , xft_ascent'
              , xft_descent
              , xft_descent'
              , xft_height
              , xft_height'
              )

where

import Graphics.X11
import Graphics.X11.Xlib.Types
import Graphics.X11.Xrender

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Codec.Binary.UTF8.String as UTF8
import Data.Char (ord)

#include <X11/Xft/Xft.h>

-- Color Handling

newtype AXftColor = AXftColor (Ptr AXftColor)

foreign import ccall "XftColorAllocName"
    cXftColorAllocName :: Display -> Visual -> Colormap -> CString -> AXftColor -> IO (#type Bool)

-- this is the missing bit in X11.Xft, not implementable from the
-- outside because XftColor does not export a constructor.
mallocAXftColor :: Display -> Visual -> Colormap -> String -> IO AXftColor
mallocAXftColor d v cm n = do
  color <- mallocBytes (#size XftColor)
  withCAString n $ \str -> cXftColorAllocName d v cm str (AXftColor color)
  return (AXftColor color)

foreign import ccall "XftColorFree"
  freeAXftColor :: Display -> Visual -> Colormap -> AXftColor -> IO ()

-- Font handling

newtype AXftFont = AXftFont (Ptr AXftFont)

xft_ascent :: AXftFont -> IO Int
xft_ascent (AXftFont p) = peekCUShort p #{offset XftFont, ascent}

xft_ascent' :: [AXftFont] -> IO Int
xft_ascent' = (fmap maximum) . (mapM xft_ascent)

xft_descent :: AXftFont -> IO Int
xft_descent (AXftFont p) = peekCUShort p #{offset XftFont, descent}

xft_descent' :: [AXftFont] -> IO Int
xft_descent' = (fmap maximum) . (mapM xft_descent)

xft_height :: AXftFont -> IO Int
xft_height (AXftFont p) = peekCUShort p #{offset XftFont, height}

xft_height' :: [AXftFont] -> IO Int
xft_height' = (fmap maximum) . (mapM xft_height)

foreign import ccall "XftTextExtentsUtf8"
  cXftTextExtentsUtf8 :: Display -> AXftFont -> CString -> CInt -> Ptr XGlyphInfo -> IO ()

xftTxtExtents :: Display -> AXftFont -> String -> IO XGlyphInfo
xftTxtExtents d f string =
    withArrayLen (map fi (UTF8.encode string)) $
    \len str_ptr -> alloca $
    \cglyph -> do
      cXftTextExtentsUtf8 d f str_ptr (fi len) cglyph
      peek cglyph

xftTxtExtents' :: Display -> [AXftFont] -> String -> IO XGlyphInfo
xftTxtExtents' d fs string = do  
    chunks <- getChunks d fs string
    let (_, _, gi, _, _) = last chunks
    return gi

foreign import ccall "XftFontOpenName"
  c_xftFontOpen :: Display -> CInt -> CString -> IO AXftFont

openAXftFont :: Display -> Screen -> String -> IO AXftFont
openAXftFont dpy screen name =
    withCAString name $
      \cname -> c_xftFontOpen dpy (fi (screenNumberOfScreen screen)) cname

foreign import ccall "XftFontClose"
  closeAXftFont :: Display -> AXftFont -> IO ()

foreign import ccall "XftCharExists"
  cXftCharExists :: Display -> AXftFont -> (#type FcChar32) -> IO (#type FcBool)

xftCharExists :: Display -> AXftFont -> Char -> IO Bool
xftCharExists d f c = bool `fmap` cXftCharExists d f (fi $ ord c)
  where
    bool 0 = False
    bool _ = True
-- Drawing

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

newtype AXftDraw = AXftDraw (Ptr AXftDraw)

foreign import ccall "XftDrawCreate"
  c_xftDrawCreate :: Display -> Drawable -> Visual -> Colormap -> IO AXftDraw

foreign import ccall "XftDrawDisplay"
  c_xftDrawDisplay :: AXftDraw -> IO Display

foreign import ccall "XftDrawDestroy"
  c_xftDrawDestroy :: AXftDraw -> IO ()

withAXftDraw :: Display -> Drawable -> Visual -> Colormap -> (AXftDraw -> IO a) -> IO a
withAXftDraw d p v c act = do
  draw <- c_xftDrawCreate d p v c
  a <- act draw
  c_xftDrawDestroy draw
  return a

foreign import ccall "XftDrawStringUtf8"
  cXftDrawStringUtf8 :: AXftDraw -> AXftColor -> AXftFont -> CInt -> CInt -> Ptr (#type FcChar8) -> CInt -> IO ()

drawXftString :: (Integral a1, Integral a) =>
                 AXftDraw -> AXftColor -> AXftFont -> a -> a1 -> String -> IO ()
drawXftString d c f x y string =
    withArrayLen (map fi (UTF8.encode string))
      (\len ptr -> cXftDrawStringUtf8 d c f (fi x) (fi y) ptr (fi len))

drawXftString' :: (Integral a1, Integral a) =>
                  AXftDraw -> AXftColor -> [AXftFont] -> a -> a1 -> String -> IO ()
drawXftString' d c fs x y string = do
    display <- c_xftDrawDisplay d
    chunks <- getChunks display fs string
    mapM_ (\(f, s, gi, xo, yo) -> drawXftString d c f (x+xo) (y+yo) s) chunks

-- Split string and determine fonts/offsets for individual parts
getChunks disp fts str = do
    chunks <- getFonts disp fts str
    getOffsets disp (XGlyphInfo 0 0 0 0 0 0) chunks
  where
    -- Split string and determine fonts for individual parts
    getFonts _ [] _ = return []
    getFonts _ _ [] = return []
    getFonts _ [ft] str = return [(ft, str)]
    getFonts disp fonts@(ft:fts) str = do
        -- Determine which glyph can be rendered by current font
        glyphs <- mapM (xftCharExists disp ft) str
        -- Split string into parts that can/cannot be rendered
        let splits = split (runs glyphs) str
        -- Determine which font to render each chunk with
        concat `fmap` mapM (getFont disp fonts) splits

    -- Determine fonts for substrings
    getFont _ [ft] (_, str) = return [(ft, str)] -- Last font, use it
    getFont _ (ft:fts) (True, str) = return [(ft, str)] -- Current font can render this substring
    getFont disp (ft:fts) (False, str) = getFonts disp fts str -- Fallback to next font

    -- Helpers
    runs [] = []
    runs (x:xs) = let (h, t) = span (==x) xs in (x, length h + 1) : runs t
    split [] _ = []
    split ((x, c):xs) str = let (h, t) = splitAt c str in (x, h) : split xs t

    -- Determine coordinates for chunks using extents
    getOffsets disp _ [] = return []
    getOffsets disp (XGlyphInfo w h x y xo yo) ((f, s):chunks) = do
        (XGlyphInfo w' h' x' y' xo' yo') <- xftTxtExtents disp f s
        let gi = XGlyphInfo (xo+w') (yo+h') x y (xo+xo') (yo+yo')
        rest <- getOffsets disp gi chunks
        return $ (f, s, gi, fromIntegral xo, fromIntegral yo) : rest

foreign import ccall "XftDrawRect"
  cXftDrawRect :: AXftDraw -> AXftColor -> CInt -> CInt -> CUInt -> CUInt -> IO ()

drawXftRect :: (Integral a3, Integral a2, Integral a1, Integral a) =>
               AXftDraw -> AXftColor -> a -> a1 -> a2 -> a3 -> IO ()
drawXftRect draw color x y width height =
  cXftDrawRect draw color (fi x) (fi y) (fi width) (fi height)
