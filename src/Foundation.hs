{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import           ClassyPrelude.Yesod

import           Database.Persist.Sql
import           Network.HTTP.Client
import           Text.Hamlet
import           Yesod.Auth
import           Yesod.Auth.HashDB
import           Yesod.Auth.Message
import           Yesod.Core
import           Yesod.Core.Types
import           Yesod.Form
import           Yesod.Static

import           Model
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
    /        HomeR     GET
    /auth    SigninR   Auth getAuth
    /profile ProfileR  GET
  |]

type Form a = Html -> MForm (HandlerFor App) (FormResult a, Widget)

type DB a = forall (m :: * -> *). (MonadIO m) => ReaderT SqlBackend m a

instance Yesod App where
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing   -> getApprootText guessApproot app req
      Just root -> root
  makeSessionBackend _ = Just <$> defaultClientSessionBackend (60 * 5) "config/client-session-key.aes"
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    maut <- maybeAuth
    mmessage <- getMessage
    pagecontent <- widgetToPageContent $ do
      $(widgetFile "def")
    withUrlRenderer $(hamletFile "templates/wrapper.hamlet")
  authRoute _ = Just $ SigninR LoginR
  isAuthorized (SigninR _) _ = return Authorized
  isAuthorized HomeR _       = return Authorized
  isAuthorized ProfileR _    = isLoggedIn

isLoggedIn :: Handler AuthResult
isLoggedIn = do
  maut <- maybeAuth
  case maut of
    Nothing -> return $ Unauthorized "login please"
    Just _  -> return Authorized

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnectionPool master

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
  type AuthId App = UsersId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  redirectToReferer _ = False
  authPlugins _ = [authHashDB (Just . UniqueUsername)]
  authenticate creds = liftHandler $ runDB $ do
    x <- getBy $ UniqueUsername $ credsIdent creds
    case x of
      Nothing             -> return $ UserError InvalidLogin
      Just (Entity uid _) -> return $ Authenticated uid

instance YesodAuthPersist App
