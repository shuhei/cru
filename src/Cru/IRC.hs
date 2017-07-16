module Cru.IRC
    ( start
    ) where

import Control.Exception (bracket, bracket_)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Data.Bifunctor (first)
import Data.List (isPrefixOf, foldl')
import Network (connectTo, PortID(..))
import Text.Printf (printf, hPrintf)
import System.Exit (exitSuccess)
import System.IO
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, NominalDiffTime)

-- Data

data Bot
  = Bot { socket :: Handle
        , startTime :: UTCTime
        }

type Net = ReaderT Bot IO

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
  t <- getCurrentTime
  h <- connectTo server $ PortNumber (fromIntegral port)
  hSetBuffering h NoBuffering
  return $ Bot h t
  where
    notify = bracket_
      (printf "Connecting to %s ..." server >> hFlush stdout)
      (putStrLn "done.")

run :: Net ()
run = do
  write "NICK" nick
  write "USER" $ nick ++ " 0 * :tutorial bot"
  write "JOIN" chan
  asks socket >>= listen

write :: String -> String -> Net ()
write s t = do
  h <- asks socket
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
eval "!uptime" = uptime >>= privmsg
eval x
  | "!id " `isPrefixOf` x = privmsg $ drop 4 x
eval _ = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" $ chan ++ " :" ++ s

uptime :: Net String
uptime = do
  now <- liftIO getCurrentTime
  zero <- asks startTime
  return . pretty $ diffUTCTime now zero

pretty :: NominalDiffTime -> String
pretty dt =
  unwords $ fmap (uncurry (++) . first show) $
  if null diffs then [(0 :: Int, "s")] else diffs
    where
      merge (tot, acc) (sec, typ) =
        let (sec', tot') = divMod tot sec
        in (tot', (sec', typ) : acc)
      metrics = [(86400, "d"), (3600, "h"), (60, "m"), (1, "s")]
      diffs = filter ((/= 0) . fst) $ reverse $ snd $ foldl' merge (floor dt, []) metrics
