{-# LANGUAGE OverloadedStrings #-}
module Cru.Types
  ( WSMessage(..)
  ) where

import Data.Aeson
import qualified Data.Text as T
import Network.Irc.Types

-- TODO: This doesn't work somehow...
{-# ANN module ("HLint: ignore Orphan instance" :: String) #-}

data WSMessage
  = WSTextMessage String
  | WSConnectionClose
  deriving (Show, Eq)

instance ToJSON Nickname where
  toJSON (Nickname nn) = toJSON nn

instance ToJSON Hostname where
  toJSON (Hostname hn) = toJSON hn

instance ToJSON Target where
  toJSON (NickTarget nickname) = withType "Nick" ["value" .= nickname]
  toJSON (ServerTarget hostname) = withType "Server" ["value" .= hostname]

instance ToJSON Reply where
  toJSON WelcomeReply = withType "Welcome" []
  toJSON _ = withType "Other" []

instance ToJSON SpecificReply where
  toJSON (SpecificReply hostname target reply) =
    object
      [ "hostname" .= hostname
      , "target" .= target
      , "reply" .= reply
      ]

instance ToJSON Username where
  toJSON (Username un) = toJSON un

instance ToJSON Address where
  toJSON (IPv4 ip) = withType "v4" ["ip" .= ip]
  toJSON (IPv6 ip) = withType "v6" ["ip" .= ip]

instance ToJSON Host where
  toJSON (HostByName hostname) = withType "ByName" ["hostname" .= hostname]
  toJSON (HostByAddr address) = withType "ByAddr" ["address" .= address]
  toJSON (HostCloak cloak) = withType "Cloak" ["cloak" .= cloak]

instance ToJSON Prefix where
  toJSON (PrefixServer hostname) =
    withType "Server" ["hostname" .= hostname]
  toJSON (PrefixNick nickname username host) =
    withType "Nick"
      [ "nickname" .= nickname
      , "username" .= username
      , "host" .= host
      ]

-- TODO: Encode messages.
instance ToJSON Message where
  toJSON _ = withType "Other" []

instance ToJSON SpecificMessage where
  toJSON (SpecificMessage prefix message) =
    object ["prefix" .= prefix , "message" .= message]

withType :: T.Text -> [(T.Text, Value)] -> Value
withType t kvs =
  object $ ("type" .= t) : kvs
