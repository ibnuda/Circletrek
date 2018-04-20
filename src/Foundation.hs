{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Foundation where

import           ClassyPrelude
import           ClassyPrelude.Yesod

import           Database.Persist.Sql
import           Network.HTTP.Client
import           Text.Hamlet
import           Yesod.Core
import           Yesod.Core.Types
import           Yesod.Form
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

type Form a = Html -> MForm (HandlerFor App) (FormResult a, Widget)

type DB a = forall (m :: * -> *). (MonadIO m) => ReaderT SqlBackend m a

instance Yesod App where
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing   -> getApprootText guessApproot app req
      Just root -> root
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmessage <- getMessage
    mcurrentroute <- getCurrentRoute
    pagecontent <- widgetToPageContent $ do
      [whamlet|
        $maybe route <- mcurrentroute
          <p> You're at #{show route}.
        $nothing
          <p> You're lost.
        ^{widget}
      |]
    withUrlRenderer $(hamletFile "templates/wrapper.hamlet")
