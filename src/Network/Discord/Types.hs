{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Network.Discord.Types
  ( module Network.Discord.Types
  , module Network.Discord.Types.Json
  ) where

  import Network.WebSockets (Connection)

  import Network.Discord.Types.Json

  type Auth = String

  class Client c where
    getAuth :: Client c => c -> Auth

  data StateEnum = Create | Start | Ready | InvalidReconnect | InvalidDead

  data DiscordState a = Client a => DiscordState {
    getState     :: StateEnum,
    getClient    :: a,
    getWebSocket :: Connection
  }
