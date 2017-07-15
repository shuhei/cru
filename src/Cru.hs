module Cru
    ( start
    ) where

import Control.Exception (bracket, bracket_)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.List (isPrefixOf)
import Network (connectTo, PortID(..))
import Text.Printf (printf, hPrintf)
import System.IO
import System.Exit (exitSuccess)

-- Data

data Bot = Bot { socket :: Handle }

type Net = ReaderT Bot IO

askSocket :: Net Handle
askSocket = asks socket

-- Configs

server :: String
server = "irc.freenode.org"

port :: Int
port = 6667

chan :: String
chan = "#tutbot-testing"

nick :: String
nick = "tutbot"

-- IRC

start :: IO ()
start = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop = runReaderT run

connect :: IO Bot
connect = notify $ do
  h <- connectTo server $ PortNumber (fromIntegral port)
  hSetBuffering h NoBuffering
  return $ Bot h
  where
    notify = bracket_
      (printf "Connecting to %s ..." server >> hFlush stdout)
      (putStrLn "done.")

run :: Net ()
run = do
  write "NICK" nick
  write "USER" $ nick ++ " 0 * :tutorial bot"
  write "JOIN" chan
  askSocket >>= listen

write :: String -> String -> Net ()
write s t = do
  h <- askSocket
  liftIO $ do
    hPrintf h "%s %s\r\n" s t
    printf "> %s %s\n" s t

listen :: Handle -> Net ()
listen h = forever $ do
  s <- liftIO $ init <$> hGetLine h
  liftIO $ putStrLn s
  if ping s
  then pong s
  else eval $ clean s
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write "PONG" (':' : drop 6 x)

-- Clean prefix and get the actual messge from a line.
-- This will get "hey" from the following example line:
-- :shuhei!051c50d7@gateway/web/freenode/ip.5.28.80.215 PRIVMSG #tutbot-testing :hey
clean :: String -> String
clean = drop 1 . dropWhile (/= ':') . drop 1

eval :: String -> Net ()
eval "!quit" = do
  write "QUIT" ":Exiting"
  liftIO exitSuccess
eval x
  | "!id " `isPrefixOf` x = privmsg $ drop 4 x
eval _ = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" $ chan ++ " :" ++ s
