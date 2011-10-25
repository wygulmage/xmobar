{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Localize
-- Copyright   :  (C) 2011 Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides an interface to locale information e.g. for DateL
--
-----------------------------------------------------------------------------

module Localize
    ( setupTimeLocale,
      getTimeLocale
    ) where

import Foreign
import Foreign.C
import qualified System.Locale as L

#ifdef UTF8
import Codec.Binary.UTF8.String
#endif

--  get localized strings
type NlItem = CInt

#include <langinfo.h>
foreign import ccall unsafe "langinfo.h nl_langinfo"
  nl_langinfo :: NlItem -> IO CString

#{enum NlItem,
  , AM_STR , PM_STR \
  , D_T_FMT , D_FMT , T_FMT , T_FMT_AMPM \
  , ABDAY_1 , ABDAY_2 , ABDAY_3 , ABDAY_4 , ABDAY_5 , ABDAY_6 , ABDAY_7 \
  , DAY_1 , DAY_2 , DAY_3 , DAY_4 , DAY_5 , DAY_6 , DAY_7 \
  , ABMON_1 , ABMON_2 , ABMON_3 , ABMON_4 , ABMON_5 , ABMON_6 , ABMON_7 \
  , ABMON_8 , ABMON_9 , ABMON_10 , ABMON_11 , ABMON_12 \
  , MON_1 , MON_2 , MON_3 , MON_4  , MON_5 , MON_6 , MON_7 , MON_8 , MON_9 \
  , MON_10 , MON_11 , MON_12\
 }

#let LIST_CTR fst,snd,idx = "( getLangInfo "fst""idx" , getLangInfo "snd""idx" )"

getLangInfo :: NlItem -> String
#ifdef UTF8
getLangInfo item = decodeString $ unsafePerformIO $ getLangInfo' item
#else
getLangInfo item = unsafePerformIO $ getLangInfo' item
#endif

getLangInfo' :: NlItem -> IO String
getLangInfo' item = do
  itemStr <- nl_langinfo item
  peekCString itemStr

#include <locale.h>
foreign import ccall unsafe "locale.h setlocale"
    setlocale :: CInt -> CString -> IO CString

setupTimeLocale :: String -> IO ()
setupTimeLocale l = withCString l (setlocale #const LC_TIME) >> return ()

getTimeLocale :: L.TimeLocale
getTimeLocale = L.TimeLocale {
    L.wDays = [ #{LIST_CTR "day","abday","1"}
              , #{LIST_CTR "day","abday","2"}
              , #{LIST_CTR "day","abday","3"}
              , #{LIST_CTR "day","abday","4"}
              , #{LIST_CTR "day","abday","5"}
              , #{LIST_CTR "day","abday","6"}
              , #{LIST_CTR "day","abday","7"}
              ],
    L.months = [ #{LIST_CTR "mon","abmon","1"}
               , #{LIST_CTR "mon","abmon","2"}
               , #{LIST_CTR "mon","abmon","3"}
               , #{LIST_CTR "mon","abmon","4"}
               , #{LIST_CTR "mon","abmon","5"}
               , #{LIST_CTR "mon","abmon","6"}
               , #{LIST_CTR "mon","abmon","7"}
               , #{LIST_CTR "mon","abmon","8"}
               , #{LIST_CTR "mon","abmon","9"}
               , #{LIST_CTR "mon","abmon","10"}
               , #{LIST_CTR "mon","abmon","11"}
               , #{LIST_CTR "mon","abmon","12"}
              ],
    -- Intervals are not available from this interface
    L.intervals = L.intervals L.defaultTimeLocale,
    L.amPm = (getLangInfo amStr, getLangInfo pmStr),
    L.dateTimeFmt = getLangInfo dTFmt,
    L.dateFmt = getLangInfo dFmt,
    L.timeFmt = getLangInfo tFmt,
    L.time12Fmt = getLangInfo tFmtAmpm
  }
