module Cru.Types
  ( WSMessage(..)
  ) where

data WSMessage
  = WSTextMessage String
  | WSConnectionClose
  deriving (Show, Eq)
