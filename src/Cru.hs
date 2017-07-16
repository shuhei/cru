module Cru where

import qualified Cru.IRC as IRC
import qualified Cru.WebSockets as WS
import Control.Concurrent (forkIO)

start :: IO ()
start = do
  forkIO WS.start
  IRC.start
