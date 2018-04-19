{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Foundation where

import           Database.Persist.Sql
import           Network.HTTP.Client
import           Yesod.Core
import           Yesod.Core.Types
import           Yesod.Static

import           Settings

data App = App
  { appSettings       :: ApplicationSettings
  , appConnectionPool :: ConnectionPool
  , appLogger         :: Logger
  , appStatic         :: Static
  , appHttpManager    :: Manager
  }

mkYesodData
  "App"
  [parseRoutes|
    / HomeR GET
  |]

instance Yesod App where
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing   -> getApprootText guessApproot app req
      Just root -> root
