{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings where

import           ClassyPrelude.Yesod

import           Control.Exception           as Exception
import           Data.Aeson
import           Data.FileEmbed
import           Data.Yaml
import           Database.Persist.Postgresql
import           Language.Haskell.TH.Syntax
import           Network.Wai.Handler.Warp
import           Yesod.Default.Config2

data ApplicationSettings = ApplicationSettings
  { appStaticDir              :: String
  , appRoot                   :: Maybe Text
  , appHost                   :: HostPreference
  , appDatabaseConf           :: PostgresConf
  , appPort                   :: Int
  , appReloadTemplate         :: Bool
  , appMutableStatic          :: Bool
  , appSkipCombining          :: Bool
  , appDetailedRequestLogging :: Bool
  }


instance FromJSON ApplicationSettings where
  parseJSON =
    withObject "ApplicationSettings" $ \ob -> do
      let defEnv = True
      appStaticDir <- ob .: "static-dir"
      appRoot <- ob .:? "app-root"
      appHost <- fromString <$> ob .: "app-host"
      appDatabaseConf <- ob .: "database-conf"
      appPort <- ob .: "app-port"
      dev <- ob .: "development" .!= defEnv
      appReloadTemplate <- ob .:? "reload-template" .!= dev
      appMutableStatic <- ob .:? "mutable-static" .!= dev
      appSkipCombining <- ob .:? "skip-combining" .!= dev
      appDetailedRequestLogging <- ob .:? "detailed-req-log" .!= dev
      return ApplicationSettings {..}

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id $ decodeEither' configSettingsYmlBS
