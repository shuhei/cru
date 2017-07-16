module Cru where

import qualified Cru.IRC as IRC
import qualified Cru.Web as W
import qualified Cru.WebSockets as WS
import Control.Concurrent (forkIO)

start :: IO ()
start = do
  forkIO WS.start
  forkIO W.start
  IRC.start
