{-# LANGUAGE OverloadedStrings #-}
module Cru.Web
  ( start
  ) where

import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as WP
import qualified Network.Wai.Application.Static as S

start :: IO ()
start = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  WP.run port staticApp

staticApp :: W.Application
staticApp =
  S.staticApp $ S.defaultWebAppSettings "public"
