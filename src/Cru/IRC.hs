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
import Data.List (isPrefixOf)
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
         , _out :: TC.TChan T.Text
         }

makeConfig :: Handle -> String -> String -> TC.TChan T.Text -> Config
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
  Config h nick@(Nickname name) chan outgoing <- ask
  write $ NickMessage nick
  write $ UserMessage (Username name) False False (RealName "tutorial bot")
  write $ JoinMessage (Just ([chan], []))
  liftIO . STM.atomically $ TC.writeTChan outgoing "loggedin"
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
  s <- liftIO $ init <$> hGetLine h
  liftIO $ putStrLn s
  if ping s
  then pong s
  else forward out . T.pack $ clean s
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write $ PongMessage (Hostname (T.pack $ ':' : drop 6 x)) Nothing
    forward o s = liftIO . STM.atomically $ TC.writeTChan o s
    -- Clean prefix and get the actual messge from a line.
    -- This will get "hey" from the following example line:
    -- :shuhei!051c50d7@gateway/web/freenode/ip.5.28.80.215 PRIVMSG #tutbot-testing :hey
    clean = drop 1 . dropWhile (/= ':') . drop 1

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
