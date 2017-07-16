{-# LANGUAGE OverloadedStrings #-}
module Cru.Web
  ( app
  ) where

import qualified Network.Wai as W
import qualified Network.Wai.Application.Static as S

app :: W.Application
app =
  let settings = S.defaultWebAppSettings "front/build"
  in S.staticApp settings
