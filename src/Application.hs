{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import           Control.Monad
import           Control.Monad.Logger
import           Database.Persist.Postgresql
import           Foundation
import           Language.Haskell.TH.Syntax
import           Network.HTTP.Client.TLS
import           Network.Wai.Handler.Warp
import           System.Log.FastLogger
import           Yesod.Core
import           Yesod.Default.Config2
import           Yesod.Static

import           Home
import           Settings                    (ApplicationSettings (..),
                                              configSettingsYmlValue)

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

makeApplication :: App -> IO Application
makeApplication app = do
  commonapp <- toWaiApp app
  return $ defaultMiddlewaresNoLogging commonapp

newMain :: IO ()
newMain = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  app <- makeFoundation settings
  commonapp <- makeApplication app
  runSettings (warpSettings app) commonapp
