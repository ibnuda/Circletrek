{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import           Control.Monad
import           Control.Monad.Logger
import           Foundation
import           Language.Haskell.TH.Syntax
import           Network.Wai.Handler.Warp
import           Yesod.Core

import           Home
import           Settings                   (ApplicationSettings (..))

mkYesodDispatch "App" resourcesApp

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
  let app = undefined
  commonapp <- makeApplication app
  runSettings (warpSettings app) commonapp
