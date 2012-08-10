-----------------------------------------------------------------------------
-- |
-- Module      :  DBus
-- Copyright   :  (c) Jochen Keil
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jochen Keil <jochen dot keil at gmail dot com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- DBus IPC module for Xmobar
--
-----------------------------------------------------------------------------

module IPC.DBus ( runIPC ) where

import DBus
import DBus.Client
import Control.Monad (join, when)
import Control.Concurrent

import Signal
import Plugins.Utils (safeHead)

busName :: BusName
busName = busName_ "org.Xmobar.Control"

objectPath :: ObjectPath
objectPath = objectPath_ "/org/Xmobar/Control"

interfaceName :: InterfaceName
interfaceName = interfaceName_ "org.Xmobar.Control"

runIPC :: MVar SignalType -> IO ()
runIPC mvst = do
    client <- connectSession
    requestName client busName [ nameDoNotQueue ]
    export client objectPath [ sendSignalMethod mvst ]

sendSignalMethod :: MVar SignalType -> Method
sendSignalMethod mvst = method interfaceName sendSignalName
    (signature_ [variantType $ toVariant $ (undefined :: SignalType)])
    (signature_ [])
    sendSignalMethodCall
    where
    sendSignalName :: MemberName
    sendSignalName = memberName_ "SendSignal"

    sendSignalMethodCall :: MethodCall -> IO Reply
    sendSignalMethodCall mc = do
        when ( methodCallMember mc == sendSignalName ) $ sendSignal $
            join $ safeHead $ map fromVariant $ methodCallBody mc
        return ( replyReturn [] )

    sendSignal :: Maybe SignalType -> IO ()
    sendSignal = maybe (return ()) (putMVar mvst)
