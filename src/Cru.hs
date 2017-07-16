module Cru where

import qualified Cru.IRC as IRC
import qualified Cru.Web as Web
import qualified Cru.WebSockets as WS
import Control.Concurrent (forkIO)
import qualified Network.Wai.Handler.WebSockets as HWS
import qualified Network.Wai.Handler.Warp as HW
import qualified Network.WebSockets.Connection as WSC

start :: IO ()
start = do
  forkIO $ startWeb 8080
  IRC.start

startWeb :: Int -> IO ()
startWeb port = do
  let app = HWS.websocketsOr WSC.defaultConnectionOptions WS.app Web.app
  putStrLn $ "Listening on port " ++ show port
  HW.run port app
