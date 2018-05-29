{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.User where

import           Import

import           Database.Esqueleto
import           Yesod.Auth.Util.PasswordStore

import           DBOp.CRUDGroup
import           DBOp.CRUDPost
import           DBOp.CRUDTopic
import           DBOp.CRUDUser

import Flux.Miscellaneous

unusedUser ::
     Text
  -> Text
  -> Handler ()
unusedUser username email = do
  users <- liftHandler $ runDB $ selectUserByUsernameOrEmail username email
  case users of
    [] -> return ()
    _  -> invalidArgs ["Username and/or email has been used."]

getUserById ::
     Key Users
  -> Handler (Entity Users)
getUserById uid = do
  users <- liftHandler $ runDB $ selectUserById uid
  case users of
    []  -> notFound
    x:_ -> return x

registerUser :: Text -> Text -> Text -> Handler (Key Users)
registerUser username password email = do
  unusedUser username email
  now <- liftIO getCurrentTime
  gids <- liftHandler $ runDB $ selectGroupByGrouping Member
  case gids of
    [] -> invalidArgs ["Call the Administrator"]
    x:_ -> do
      password' <- liftIO $ makePassword (encodeUtf8 password) 17
      liftHandler $
        runDB $
        insertUser
          (entityKey x)
          username
          (Just $ decodeUtf8 password')
          email
          now

getUsersByConditions ::
     Maybe (Key Groups)
  -> Maybe Text
  -> Maybe Text
  -> Handler [(Entity Users, Grouping)]
getUsersByConditions mgid musername memail = do
  userandgroup <-
    liftHandler $ runDB $ selectUsersByConditions mgid musername memail
  return $ map (\(user, Value group) -> (user, group)) userandgroup

getAllUsers :: Bool -> Handler [(Grouping, Entity Users)]
getAllUsers ascending = do
  groupandusers <- liftHandler $ runDB $ selectAllUsers ascending
  return $ map (\(Value a, x) -> (a, x)) groupandusers

searchUserByConditions ::
     Maybe Text
  -> Maybe (Key Groups)
  -> SortBy
  -> Bool
  -> Handler [(Grouping, Entity Users)]
searchUserByConditions username groupid orderby ascending = do
  groupandusers <-
    liftHandler $
    runDB $ selectUsersBySearchConditions username groupid orderby ascending
  return $ map (\(Value a, x) -> (a, x)) groupandusers

selfUpdateInfoByUser ::
     Key Users -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Handler ()
selfUpdateInfoByUser userid userpass oldpass newpass email = do
  case (userpass, oldpass, newpass) of
    (Nothing, _, _) -> error "Your profile is broken. Ask admin to fix this."
    (Just _, Nothing, Nothing) ->
      liftHandler $ runDB $ updateUserEmail userid email
    (Just _, Nothing, Just _) ->
      invalidArgs
        ["You cannot update your password without providing your old password."]
    (Just _, Just _, Nothing) ->
      invalidArgs ["You cannot use an empty password."]
    (Just up, Just op, Just np) -> do
      if verifyPassword (encodeUtf8 op) (encodeUtf8 up)
        then do
          newpassword <- liftIO $ makePassword (encodeUtf8 np) 17
          liftHandler $
            runDB $ do
              updateUserEmail userid email
              updateUserPassword userid (Just $ decodeUtf8 newpassword)
        else invalidArgs ["Your password don't match with the old one."]

updateInfoByAdmin :: Key Users -> Maybe Text -> Text -> Handler ()
updateInfoByAdmin userid Nothing email = liftHandler $ runDB $ updateUserEmail userid email
updateInfoByAdmin userid (Just np) email =
  liftHandler $
  runDB $ do
    newpassword <- liftIO $ makePassword (encodeUtf8 np) 17
    updateUserEmail userid email
    updateUserPassword userid (Just $ decodeUtf8 newpassword)

getUserPosts :: Key Users -> Handler [Entity Posts]
getUserPosts = liftHandler . runDB . selectPostByPosterId

getUserTopics :: Key Users -> Handler [(Text, Entity Topics)]
getUserTopics userid = do
  topics <- liftHandler . runDB . selectTopicForumNameByPosterId $ userid
  return $ map (\(Value forumname, x) -> (forumname, x)) topics
