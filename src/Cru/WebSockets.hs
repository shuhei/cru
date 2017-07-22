{-# LANGUAGE OverloadedStrings #-}
module Cru.WebSockets
  ( app
  ) where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Exception (try)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text.Lazy as TL
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
  (chanIRC, chanWS) <- atomically $ (,) <$> newTChan <*> newTChan
  let config = IRC.Config handle nickname channel chanIRC

  let irc = race (IRC.runIRC IRC.reader config)
                 (IRC.runIRC (IRC.writer chanWS) config)
  let ws = race (writer chanIRC conn)
                (reader chanWS conn)
  race ws irc
  return ()

writer :: TChan String -> WS.Connection -> IO ()
writer incoming conn = forever $ do
  s <- atomically $ readTChan incoming
  WS.sendTextData conn $ BSC.pack s

waitLogin :: WS.Connection -> IO (String, String)
waitLogin conn = do
  m <- extractMessage <$> WS.receiveDataMessage conn
  if m == "login"
  then return ("tutbot", "#tutbot-testing")
  else waitLogin conn

reader :: TChan WSMessage -> WS.Connection -> IO ()
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
      atomically $ writeTChan outgoing m

-- TODO: Write message decoder
extractMessage :: WS.DataMessage -> String
extractMessage (WS.Text txt) = TL.unpack $ decodeUtf8 txt
extractMessage (WS.Binary _) = "binary!"
