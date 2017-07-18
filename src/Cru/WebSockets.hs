{-# LANGUAGE OverloadedStrings #-}
module Cru.WebSockets
  ( app
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.WebSockets as WS

app :: WS.ServerApp
app pc = do
  conn <- WS.acceptRequest pc
  WS.sendTextData conn ("connected" :: ByteString)
  (nickname, channel) <- waitLogin conn
  print (nickname, channel)
  return ()

waitLogin :: WS.Connection -> IO (String, String)
waitLogin conn = do
  m <- extractMessage <$> WS.receiveDataMessage conn
  if m == "login"
  then return ("tutbot", "#tutbot-testing")
  else waitLogin conn

-- TODO: Write message decoder
extractMessage :: WS.DataMessage -> Text
extractMessage (WS.Text txt) = decodeUtf8 txt
extractMessage (WS.Binary _) = "binary!"
