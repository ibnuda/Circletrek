{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Post where

import           Import             hiding (Value)

import           Database.Esqueleto
import           DBOp.CRUDPost
import           DBOp.CRUDReport
import           DBOp.CRUDTopic

getPostsInTopic :: Key Topics -> Int64 -> Handler [Entity Posts]
getPostsInTopic tid page
  | page < 1 = invalidArgs ["Have you seen something page 0 before?"]
  | otherwise = do
    posts <- liftHandler $ runDB $ selectPostByTopicId tid page
    case posts of
      [] -> notFound -- because there's none. lol.
      _  -> return posts

getPostById :: Key Posts -> Handler (Entity Posts)
getPostById pid
  | pid < (toSqlKey 1) = invalidArgs ["Please..."]
  | otherwise = do
    posts <- liftHandler $ runDB $ selectPostById pid
    case posts of
      []  -> notFound
      x:_ -> return x

getPostParentInformation ::
     Key Posts
  -> Handler (Value Text, Value (Key Forums), Value Text, Entity Posts)
getPostParentInformation pid = do
  postandparent <- liftHandler $ runDB $ selectPostAndItsParentsInfo pid
  case postandparent of
    []  -> notFound
    x:_ -> return x

editPostByUidGroupAndContent ::
     Key Users -> Grouping -> Key Posts -> Key Users -> Text -> Handler ()
editPostByUidGroupAndContent _ group pid _ content
  | group == Administrator || group == Moderator =
    liftHandler $ runDB $ updatePostContent pid content
editPostByUidGroupAndContent _ Banned _ _ _ =
  permissionDenied "Bruh... You've been banned. Please..."
editPostByUidGroupAndContent uid Member pid uid' content
  | uid /= uid' = permissionDenied "You're not allowed to edit this post."
  | otherwise = liftHandler $ runDB $ updatePostContent pid content

createReport ::
     Key Posts -> Key Topics -> Key Forums -> Key Users -> Text -> Handler ()
createReport pid tid fid uid message = do
  now <- liftIO getCurrentTime
  liftHandler $ runDB $ insertReport pid tid fid uid now message Nothing Nothing
