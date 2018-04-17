{-# LANGUAGE CPP #-}
module Settings where

import           ClassyPrelude.Yesod
import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp

data ApplicationSettings = ApplicationSettings
  { appStaticDir              :: String
  , appRoot                   :: Maybe Text
  , appHost                   :: HostPreference
  , appPort                   :: Int
  , appReloadTemplate         :: Bool
  , appMutableStatic          :: Bool
  , appSkipCombining          :: Bool
  , appDetailedRequestLogging :: Bool
  }
