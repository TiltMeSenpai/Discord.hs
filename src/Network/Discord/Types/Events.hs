{-# LANGUAGE OverloadedStrings, GADTs #-}
module Network.Discord.Types.Events where
  import Control.Monad (mzero)

  import Data.Aeson

  import Network.Discord.Types.Json


  data Init = Init Int User [Channel] [Guild] String
  instance FromJSON Init where
    parseJSON (Object o) = Init <$> o .: "v"
                                <*> o .: "user"
                                <*> o .: "private_channels"
                                <*> o .: "guilds"
                                <*> o .: "session_id"
    parseJSON _          = mzero

  data Event a =
      Ready Init
    | Resumed                 Object
    | ChannelCreate           Channel
    | ChannelUpdate           Channel
    | ChannelDelete           Channel
    | GuildCreate             Guild
    | GuildUpdate             Guild
    | GuildDelete             Guild
    | GuildBanAdd             Member
    | GuildBanRemove          Member
    | GuildEmojisUpdate       Object
    | GuildIntegrationsUpdate Object
    | GuildMemberAdd          Member
    | GuildMemberRemove       Member
    | GuildMemberUpdate       Member
    | GuildMembersChunk       Object
    | GuildRoleCreate         Object
    | GuildRoleUpdate         Object
    | GuildRoleDelete         Object
    | MessageCreate           Message
    | MessageUpdate           Message
    | MessageDelete           Object
    | MessageDeleteBulk       Object
    | PresenceUpdate          Object
    | TypingStart             Object
    | UserSettingsUpdate      Object
    | UserUpdate              Object
    | VoiceStateUpdate        Object
    | VoiceServerUpdate       Object
