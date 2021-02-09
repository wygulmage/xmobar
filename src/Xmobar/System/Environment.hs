-----------------------------------------------------------------------------
-- |
-- Module      :  XMobar.Environment
-- Copyright   :  (c) William Song
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Will Song <incertia@incertia.net>
-- Stability   :  stable
-- Portability :  portable
--
-- A function to expand environment variables in strings
--
-----------------------------------------------------------------------------
module Xmobar.System.Environment(expandEnv) where

import Data.Maybe (fromMaybe)
import System.Environment   (lookupEnv)

expandEnv :: String -> IO String
expandEnv "" = return ""
expandEnv (c:s) = case c of
  '$'       -> do
    envVar <- fromMaybe "" <$> lookupEnv e
    remainder <- expandEnv s'
    return $ envVar ++ remainder
    where (e, s') = getVar s
          getVar ('{' : s'') = fmap (drop 1) (span (/= '}') s'')
          getVar s'' = span (`notElem` filterstr) s''
          filterstr = ",./? \t;:\"'~`!@#$%^&*()<>-+=\\|"

  '\\' -> case s == "" of
    True  -> return "\\"
    False -> do
      remainder <- expandEnv $ drop 1 s
      return $ escString s ++ remainder
      where escString s' = let (cc:_) = s' in
              case cc of
                't' -> "\t"
                'n' -> "\n"
                '$' -> "$"
                _   -> [cc]

  _    -> do
    remainder <- expandEnv s
    return $ c : remainder
