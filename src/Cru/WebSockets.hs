{-# LANGUAGE OverloadedStrings #-}
module Cru.WebSockets
  ( app
  ) where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM.TChan as TC
import Control.Exception (try)
import Control.Monad (forever)
import qualified Control.Monad.STM as STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.WebSockets as WS
import qualified Cru.IRC as IRC
import Cru.Types

-- WebSockets

app :: WS.ServerApp
app pc = do
  conn <- WS.acceptRequest pc
  WS.sendTextData conn ("connected" :: ByteString)
  (nickname, channel) <- waitLogin conn
  print (nickname, channel)

  handle <- IRC.connect
  (chanIRC, chanWS) <- STM.atomically $ (,) <$> TC.newTChan <*> TC.newTChan
  -- TODO: Stop threads somehow. Kill from outside or stop inside with a TVar?
  let config = IRC.Config handle nickname channel chanIRC
  CC.forkIO $ IRC.runIRC IRC.reader config
  CC.forkIO $ IRC.runIRC (IRC.writer chanWS) config

  CC.forkIO $ writer chanIRC conn
  reader chanWS conn

writer :: TC.TChan String -> WS.Connection -> IO ()
writer incoming conn = forever $ do
  s <- STM.atomically $ TC.readTChan incoming
  WS.sendTextData conn $ BSC.pack s

waitLogin :: WS.Connection -> IO (String, String)
waitLogin conn = do
  m <- extractMessage <$> WS.receiveDataMessage conn
  if m == "login"
  then return ("tutbot", "#tutbot-testing")
  else waitLogin conn

reader :: TC.TChan WSMessage -> WS.Connection -> IO ()
reader outgoing conn = do
  lr <- try $ WSTextMessage . extractMessage <$> WS.receiveDataMessage conn
  case lr of
    Right tm -> do
      forward tm
      reader outgoing conn
    -- TODO: Actually close WS connection! Closed by the library?
    Left (WS.CloseRequest _ _) -> forward WSConnectionClose
    Left _ -> forward WSConnectionClose
  where
    forward m = do
      print m
      STM.atomically $ TC.writeTChan outgoing m

-- TODO: Write message decoder
extractMessage :: WS.DataMessage -> String
extractMessage (WS.Text txt) = LT.unpack $ decodeUtf8 txt
extractMessage (WS.Binary _) = "binary!"
