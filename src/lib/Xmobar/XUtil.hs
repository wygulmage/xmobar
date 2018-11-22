{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XUtil
-- Copyright   :  (C) 2011, 2012, 2013, 2014, 2015, 2017, 2018 Jose Antonio Ortega Ruiz
--                (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Xmobar.XUtil
    ( XFont
    , initFont
    , initCoreFont
    , initUtf8Font
    , textExtents
    , textWidth
    , printString
    , nextEvent'
    , readFileSafe
    , hGetLineSafe
    ) where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans
import Control.Exception (SomeException, handle)
import Data.List
import Foreign
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import qualified Graphics.X11.Xlib as Xlib (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import System.Mem.Weak ( addFinalizer )
import System.Posix.Types (Fd(..))
import System.IO

#if defined XFT || defined UTF8
import qualified System.IO as S (readFile,hGetLine)
#endif

#if defined XFT
import Xmobar.MinXft
import Graphics.X11.Xrender
#endif

import Xmobar.ColorCache

readFileSafe :: FilePath -> IO String
#if defined XFT || defined UTF8
readFileSafe = S.readFile
#else
readFileSafe = readFile
#endif

hGetLineSafe :: Handle -> IO String
#if defined XFT || defined UTF8
hGetLineSafe = S.hGetLine
#else
hGetLineSafe = hGetLine
#endif

-- Hide the Core Font/Xft switching here
data XFont = Core FontStruct
           | Utf8 FontSet
#ifdef XFT
           | Xft  [AXftFont]
#endif

-- | When initFont gets a font name that starts with 'xft:' it switchs
-- to the Xft backend Example: 'xft:Sans-10'
initFont :: Display -> String -> IO XFont
initFont d s =
       let xftPrefix = "xft:" in
       if  xftPrefix `isPrefixOf` s then
#ifdef XFT
           fmap Xft $ initXftFont d s
#else
           do
               hPutStrLn stderr $ "Warning: Xmobar must be built with "
                   ++ "the with_xft flag to support font '" ++ s
                   ++ ".' Falling back on default."
               initFont d miscFixedFont
#endif
       else
           fmap Utf8 $ initUtf8Font d s

miscFixedFont :: String
miscFixedFont = "-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*"

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initCoreFont :: Display -> String -> IO FontStruct
initCoreFont d s = do
  f <- handle fallBack getIt
  addFinalizer f (freeFont d f)
  return f
      where getIt = loadQueryFont d s
            fallBack :: SomeException -> IO FontStruct
            fallBack = const $ loadQueryFont d miscFixedFont

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initUtf8Font :: Display -> String -> IO FontSet
initUtf8Font d s = do
  (_,_,f) <- handle fallBack getIt
  addFinalizer f (freeFontSet d f)
  return f
      where getIt = createFontSet d s
            fallBack :: SomeException -> IO ([String], String, FontSet)
            fallBack = const $ createFontSet d miscFixedFont

#ifdef XFT
initXftFont :: Display -> String -> IO [AXftFont]
initXftFont d s = do
  let fontNames = wordsBy (== ',') (drop 4 s)
  mapM openFont fontNames
  where
    openFont fontName = do
        f <- openAXftFont d (defaultScreenOfDisplay d) fontName
        addFinalizer f (closeAXftFont d f)
        return f
    wordsBy p str = case dropWhile p str of
                        ""   -> []
                        str' -> w : wordsBy p str''
                                where
                                    (w, str'') = break p str'
#endif

textWidth :: Display -> XFont -> String -> IO Int
textWidth _   (Utf8 fs) s = return $ fromIntegral $ wcTextEscapement fs s
textWidth _   (Core fs) s = return $ fromIntegral $ Xlib.textWidth fs s
#ifdef XFT
textWidth dpy (Xft xftdraw) s = do
    gi <- xftTxtExtents' dpy xftdraw s
    return $ xglyphinfo_xOff gi
#endif

textExtents :: XFont -> String -> IO (Int32,Int32)
textExtents (Core fs) s = do
  let (_,a,d,_) = Xlib.textExtents fs s
  return (a,d)
textExtents (Utf8 fs) s = do
  let (_,rl)  = wcTextExtents fs s
      ascent  = fromIntegral $ - (rect_y rl)
      descent = fromIntegral $ rect_height rl + fromIntegral (rect_y rl)
  return (ascent, descent)
#ifdef XFT
textExtents (Xft xftfonts) _ = do
  ascent  <- fromIntegral `fmap` xft_ascent'  xftfonts
  descent <- fromIntegral `fmap` xft_descent' xftfonts
  return (ascent, descent)
#endif

printString :: Display -> Drawable -> XFont -> GC -> String -> String
            -> Position -> Position -> String -> Int -> IO ()
printString d p (Core fs) gc fc bc x y s a = do
    setFont d gc $ fontFromFontStruct fs
    withColors d [fc, bc] $ \[fc', bc'] -> do
      setForeground d gc fc'
      when (a == 255) (setBackground d gc bc')
      drawImageString d p gc x y s

printString d p (Utf8 fs) gc fc bc x y s a =
    withColors d [fc, bc] $ \[fc', bc'] -> do
      setForeground d gc fc'
      when (a == 255) (setBackground d gc bc')
      liftIO $ wcDrawImageString d p fs gc x y s

#ifdef XFT
printString dpy drw fs@(Xft fonts) _ fc bc x y s al =
  withDrawingColors dpy drw fc bc $ \draw fc' bc' -> do
    when (al == 255) $ do
      (a,d)  <- textExtents fs s
      gi <- xftTxtExtents' dpy fonts s
      drawXftRect draw bc' x (y - a) (1 + xglyphinfo_xOff gi) (a + d + 2)
    drawXftString' draw fc' fonts (toInteger x) (toInteger y) s
#endif

-- | A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEventPtr -> IO ()
nextEvent' d p = do
    pend <- pending d
    if pend /= 0
        then nextEvent d p
        else do
            threadWaitRead (Fd fd)
            nextEvent' d p
 where
    fd = connectionNumber d
