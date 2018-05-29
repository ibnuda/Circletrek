{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Forum where

import           Import             hiding (Value)

import           Database.Esqueleto

import           DBOp.CRUDForum
import           DBOp.CRUDPost
import           DBOp.CRUDTopic
import           DBOp.CRUDUser

import           Flux.Topic

getForumsInformation :: Key Forums -> Handler (Entity Forums)
getForumsInformation fid = do
  forum <- liftHandler $ runDB $ selectForumById fid
  case forum of
    [x] -> return x
    _   -> notFound

getTopicsInForum :: Key Forums -> Int64 -> Handler [Entity Topics]
getTopicsInForum fid page | page < 1 = invalidArgs ["Yo! Have you seen negative page before? Me neither."]
getTopicsInForum fid page = liftHandler $ runDB $ selectTopicsByForumIdPage fid page

createTopicByPosting ::
     Key Forums -> Key Users -> Text -> Text -> Text -> Handler (Key Topics)
createTopicByPosting fid userid username subject content = do
  now <- liftIO getCurrentTime
  tid <- liftHandler $ runDB $ insertTopic fid username subject
  liftHandler $ runDB $ do
    _ <- insertPost tid 1 username userid content
    updateForumIncrementTopic fid
    updateUserIncrementTopic userid
  return tid

lockUnlockTopic :: Bool -> Grouping -> Text -> Handler ()
lockUnlockTopic lock group tid
  | group == Administrator || group == Moderator = do
    topic <- getTopicById . toSqlKey . forceTextToInt64 $ tid
    if (topicsIsLocked $ entityVal topic) == lock
      then invalidArgs ["You can only switch the lock of the topic."]
      else liftHandler $
           runDB $ updateTopicIsLocked (toSqlKey . forceTextToInt64 $ tid) lock
lockUnlockTopic _ lock _ = permissionDenied "You're not allowed to lock this topic."
