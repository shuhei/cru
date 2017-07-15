module Cru
    ( run
    ) where

import Network (connectTo, PortID(..))
import System.IO
import Text.Printf (printf, hPrintf)
import Control.Monad (forever)
import Data.List (isPrefixOf)
import System.Exit (exitSuccess)

server :: String
server = "irc.freenode.org"

port :: Int
port = 6667

chan :: String
chan = "#tutbot-testing"

nick :: String
nick = "tutbot"

run :: IO ()
run = do
  h <- connectTo server $ PortNumber (fromIntegral port)
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" $ nick ++ " 0 * :tutorial bot"
  write h "JOIN" chan
  listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
  s <- init <$> hGetLine h
  if ping s
  then pong s
  else eval h $ clean s
  putStrLn s
  where
    ping x = "PING :" `isPrefixOf` x
    pong x = write h "PONG" (':' : drop 6 x)

-- Clean prefix and get the actual messge from a line.
-- This will get "hey" from the following example line:
-- :shuhei!051c50d7@gateway/web/freenode/ip.5.28.80.215 PRIVMSG #tutbot-testing :hey
clean :: String -> String
clean = drop 1 . dropWhile (/= ':') . drop 1

eval :: Handle -> String -> IO ()
eval h "!quit" = write h "QUIT" ":Exiting" >> exitSuccess
eval h x
  | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval _ _ = return ()

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" $ chan ++ " :" ++ s
