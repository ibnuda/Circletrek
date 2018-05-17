{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import           Import                               hiding (Settings (..))

import           Control.Monad.Logger
import           Database.Persist.Postgresql
import           Foundation
import           Language.Haskell.TH.Syntax
import           Network.HTTP.Client.TLS
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           System.Log.FastLogger

import           Model
import           Settings                             (ApplicationSettings (..),
                                                       configSettingsYmlValue)

import           Handler.Home
import           Handler.User
import           Handler.Adm
import           Handler.Adm.Category
import           Handler.Adm.Forum
import           Handler.Adm.Ban
import           Handler.Adm.User
import           Handler.Forum
import           Handler.Topic
import           Handler.Post

mkYesodDispatch "App" resourcesApp

makeFoundation :: ApplicationSettings -> IO App
makeFoundation appSettings = do
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings
       then staticDevel
       else static)
      (appStaticDir appSettings)
  let mkFoundation appConnectionPool = App {..}
      tempFoundation =
        mkFoundation $ error "Connection pool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger
  pool <-
    flip runLoggingT logFunc $
    createPostgresqlPool
      (pgConnStr $ appDatabaseConf appSettings)
      (pgPoolSize $ appDatabaseConf appSettings)
  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
  return $ mkFoundation pool

warpSettings :: App -> Settings
warpSettings app =
  setPort (appPort $ appSettings app) $
  setHost (appHost $ appSettings app) $
  setOnException
    (\_req exception ->
       when (defaultShouldDisplayException exception) $
       messageLoggerSource
         app
         (appLogger app)
         $(qLocation >>= liftLoc)
         "yesod"
         LevelError
         (toLogStr $ "Exception from warp: " ++ show exception))
    defaultSettings

makeLogware :: App -> IO Middleware
makeLogware app = do
  mkRequestLogger
    def
    { outputFormat =
        if appDetailedRequestLogging $ appSettings app
          then Detailed True
          else Apache FromFallback
    , destination = Logger $ loggerSet $ appLogger app
    }

makeApplication :: App -> IO Application
makeApplication app = do
  logware <- makeLogware app
  commonapp <- toWaiApp app
  return $ logware $ defaultMiddlewaresNoLogging commonapp

newMain :: IO ()
newMain = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  app <- makeFoundation settings
  commonapp <- makeApplication app
  runSettings (warpSettings app) commonapp

-- DEVEL

getAppSettings :: IO ApplicationSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

getAppDev :: IO (Settings, Application)
getAppDev = do
  settings <- getAppSettings
  found <- makeFoundation settings
  warpsettings <- getDevSettings $ warpSettings found
  app <- makeApplication found
  return (warpsettings, app)

develMain :: IO ()
develMain = develMainHelper getAppDev
