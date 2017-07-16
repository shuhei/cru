{-# LANGUAGE OverloadedStrings #-}
module Cru.WebSockets
  ( app
  ) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.WebSockets as WS

app :: WS.ServerApp
app pc = do
  conn <- WS.acceptRequest pc
  m <- extractMessage <$> WS.receiveDataMessage conn
  print m
  return ()

-- TODO: Write message decoder
extractMessage :: WS.DataMessage -> Text
extractMessage (WS.Text txt) = decodeUtf8 txt
extractMessage (WS.Binary _) = "binary!"
