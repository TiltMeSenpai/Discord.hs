{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Network.Discord.Types
  ( module Network.Discord.Types
  , module Re
  ) where

    import Control.Concurrent.STM
    import Network.WebSockets (Connection)

    import Network.Discord.Types.Json as Re
    import Network.Discord.Types.Events as Re
    import Network.Discord.Types.Gateway as Re

    type Auth = String

    class Client c where
      getAuth :: Client c => c -> Auth

    data StateEnum = Create | Start | Running | InvalidReconnect | InvalidDead

    data DiscordState a = Client a => DiscordState {
        getState       :: StateEnum
      , getClient      :: a
      , getWebSocket   :: Connection
      , getSequenceNum :: TMVar Integer
      , getRateLimits  :: [(String, Integer)]
      }
