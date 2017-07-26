{-# LANGUAGE OverloadedStrings #-}
module Cru.IRC
    ( connect
    , reader
    , writer
    , runIRC
    , Config
    , makeConfig
    ) where

import qualified Control.Concurrent.STM.TChan as TC
import Control.Exception (bracket_)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import qualified Control.Monad.STM as STM
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network (connectTo, PortID(..))
import Network.Irc.Messages as IRCM
import Network.Irc.Types hiding (PortNumber)
import Text.Printf (printf)
import System.IO
import Cru.Types

-- Types

data Config =
  Config { _socket :: Handle
         , _nickname :: Nickname
         , _channel :: Channel
         , _out :: TC.TChan (Either SpecificReply SpecificMessage)
         }

makeConfig :: Handle -> String -> String -> TC.TChan (Either SpecificReply SpecificMessage) -> Config
makeConfig h nick channel =
  Config h (Nickname $ T.pack nick) (Channel $ T.pack channel)

type Net = ReaderT Config IO

runIRC :: Net a -> Config -> IO a
runIRC = runReaderT

-- Configs

server :: String
server = "irc.freenode.org"

port :: Int
port = 6667

-- IRC

connect :: IO Handle
connect = notify $ do
  h <- connectTo server $ PortNumber (fromIntegral port)
  hSetBuffering h NoBuffering
  return h
  where
    notify = bracket_
      (printf "Connecting to %s ..." server >> hFlush stdout)
      (putStrLn "done.")

reader :: Net ()
reader = do
  Config h nick@(Nickname name) chan _outgoing <- ask
  write $ NickMessage nick
  write $ UserMessage (Username name) False False (RealName "tutorial bot")
  write $ JoinMessage (Just ([chan], []))
  listen h

write :: Message -> Net ()
write sm = do
  h <- asks _socket
  let m = serialize sm
  liftIO $ do
    TIO.hPutStr h m
    TIO.putStr $ T.concat ["> ", m]
 where
  serialize =
    IRCM.serializeMessage . IRCM.buildMessage . SpecificMessage Nothing

listen :: Handle -> Net ()
listen h = forever $ do
  out <- asks _out
  -- Remove the trailing \r
  line <- liftIO $ T.init <$> TIO.hGetLine h
  let parsed = IRCM.analyze <$> IRCM.parseMessage line
  liftIO $ do
    print line
    print parsed
  case parsed of
    Just (Right (Right (SpecificMessage _prefix (PingMessage server1 server2)))) ->
      write $ PongMessage server1 server2
    Just (Right msg) ->
      forward out msg
    Just (Left err) ->
      liftIO $ TIO.putStrLn $ T.concat ["Parse error: ", err]
    Nothing ->
      return ()
  where
    forward o = liftIO . STM.atomically . TC.writeTChan o

-- Receive WebSockets messages and forward them to IRC or close IRC connection.
writer :: TC.TChan WSMessage -> Net ()
writer incoming = do
  m <- liftIO . STM.atomically $ TC.readTChan incoming
  case m of
    WSTextMessage s -> do
      privmsg s
      writer incoming
    WSConnectionClose -> do
      h <- asks _socket
      part
      liftIO $ do
        putStrLn "Disconnecting IRC"
        hClose h
        putStrLn "Disconnected IRC"

privmsg :: String -> Net ()
privmsg s = do
  chan <- asks _channel
  write $ PrivMsgMessage (ChannelTarget chan) (MsgContent $ T.pack s)

part :: Net ()
part = do
  chan <- asks _channel
  write $ PartMessage [chan] Nothing
