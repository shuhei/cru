{-# LANGUAGE OverloadedStrings #-}
module Cru.WebSockets
  ( app
  ) where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Exception (try, finally)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.WebSockets as WS
import Network.Irc.Types
import System.IO (hClose)
import qualified Cru.IRC as IRC
import Cru.Types

-- WebSockets

app :: WS.ServerApp
app pc = do
  -- WS connection is closed by websockets library.
  conn <- WS.acceptRequest pc
  WS.sendTextData conn ("connected" :: ByteString)
  (nickname, channel) <- waitLogin conn
  putStrLn $ "Login request for " ++ show (nickname, channel)

  handle <- IRC.connect
  (chanIRC, chanWS) <- atomically $ (,) <$> newTChan <*> newTChan
  let config = IRC.makeConfig handle nickname channel chanIRC

  let irc = race (IRC.runIRC IRC.reader config)
                 (IRC.runIRC (IRC.writer chanWS) config)
  let ws = race (writer chanIRC conn)
                (reader chanWS conn)
  race ws irc `finally` hClose handle
  return ()

writer :: TChan (Either SpecificReply SpecificMessage) -> WS.Connection -> IO ()
writer incoming conn = forever $ do
  m <- atomically $ readTChan incoming
  WS.sendTextData conn $ Aeson.encode m

waitLogin :: WS.Connection -> IO (String, String)
waitLogin conn = do
  m <- extractMessage <$> WS.receiveDataMessage conn
  case words m of
    "login" : nickname : rest ->
      return (nickname, unwords rest)
    _ -> waitLogin conn

reader :: TChan WSMessage -> WS.Connection -> IO ()
reader outgoing conn = do
  lr <- try $ WSTextMessage . extractMessage <$> WS.receiveDataMessage conn
  case lr of
    Right tm -> do
      forward tm
      reader outgoing conn
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
