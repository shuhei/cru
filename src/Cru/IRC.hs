module Cru.IRC
    ( connect
    , reader
    , writer
    , runIRC
    , Config(..)
    ) where

import qualified Control.Concurrent.STM.TChan as TC
import Control.Exception (bracket_)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import qualified Control.Monad.STM as STM
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.List (isPrefixOf)
import Network (connectTo, PortID(..))
import Text.Printf (printf, hPrintf)
import System.IO (Handle, hSetBuffering, hGetLine, BufferMode(..), hFlush, hClose, stdout)
import Cru.Types

-- Types

data Config =
  Config { _socket :: Handle
         , _nickname :: String
         , _channel :: String
         , _out :: TC.TChan String
         }

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
  Config h nick chan outgoing <- ask
  write "NICK" nick
  write "USER" $ nick ++ " 0 * :tutorial bot"
  write "JOIN" chan
  liftIO . STM.atomically $ TC.writeTChan outgoing "loggedin"
  listen h

write :: String -> String -> Net ()
write s t = do
  h <- asks _socket
  liftIO $ do
    hPrintf h "%s %s\r\n" s t
    printf "> %s %s\n" s t

listen :: Handle -> Net ()
listen h = forever $ do
  out <- asks _out
  s <- liftIO $ init <$> hGetLine h
  liftIO $ putStrLn s
  if ping s
  then pong s
  else forward out $ clean s
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)
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
  write "PRIVMSG" $ chan ++ " :" ++ s

part :: Net ()
part = do
  chan <- asks _channel
  write "PART" chan
