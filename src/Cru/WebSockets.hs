{-# LANGUAGE OverloadedStrings #-}
module Cru.WebSockets
  ( app
  ) where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM.TChan as TC
import Control.Monad (forever)
import qualified Control.Monad.STM as STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.WebSockets as WS
import qualified Cru.IRC as IRC

app :: WS.ServerApp
app pc = do
  conn <- WS.acceptRequest pc
  WS.sendTextData conn ("connected" :: ByteString)
  (nickname, channel) <- waitLogin conn
  print (nickname, channel)

  handle <- IRC.connect
  (chanIRC, chanWS) <- STM.atomically $ (,) <$> TC.newTChan <*> TC.newTChan
  -- TODO: Close handle somehow.
  let config = IRC.Config handle nickname channel chanIRC
  CC.forkIO $ IRC.runIRC IRC.run config
  CC.forkIO $ IRC.runIRC (IRC.runForward chanWS) config

  CC.forkIO $ forward chanIRC conn
  waitWebSockets chanWS conn

forward :: TC.TChan String -> WS.Connection -> IO ()
forward incoming conn = forever $ do
  s <- STM.atomically $ TC.readTChan incoming
  WS.sendTextData conn $ BSC.pack s

waitLogin :: WS.Connection -> IO (String, String)
waitLogin conn = do
  m <- extractMessage <$> WS.receiveDataMessage conn
  if m == "login"
  then return ("tutbot", "#tutbot-testing")
  else waitLogin conn

waitWebSockets :: TC.TChan String -> WS.Connection -> IO ()
waitWebSockets outgoing conn = do
  m <- extractMessage <$> WS.receiveDataMessage conn
  print m
  STM.atomically $ TC.writeTChan outgoing (LT.unpack m)
  waitWebSockets outgoing conn

-- TODO: Write message decoder
extractMessage :: WS.DataMessage -> LT.Text
extractMessage (WS.Text txt) = decodeUtf8 txt
extractMessage (WS.Binary _) = "binary!"
