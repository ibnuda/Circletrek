{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDTopic where

import           Import                        hiding (Value, groupBy, on,
                                                update, (+=.), (=.), (==.))

import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL

insertTopic :: Key Forums -> Text -> Text -> DB (Key Topics)
insertTopic fid poster subject = do
  now <- liftIO getCurrentTime
  let topicsForumId = fid
      topicsPoster = poster
      topicsSubject = subject
      topicsRepliesCount = 0
      topicsStartTime = now
      topicsLastPost = Nothing
      topicsLastPostId = Nothing
      topicsLastPoster = Nothing
      topicsIsLocked = False
  insert Topics {..}

selectTopicById :: Key Topics -> DB [Entity Topics]
selectTopicById tid = do
  select $ from $ \topic -> do
    where_ (topic ^. TopicsId ==. val tid)
    limit 1
    return topic

updateTopicIsLocked :: Key Topics -> Bool -> DB ()
updateTopicIsLocked tid locked = do
  update $ \topic -> do
    set topic [TopicsIsLocked =. val locked]
    where_ (topic ^. TopicsId ==. val tid)

updateTopicIncrementReplyAndLasts ::
     Key Topics -> Text -> Key Posts -> UTCTime -> DB ()
updateTopicIncrementReplyAndLasts tid username pid now = do
  update $ \topic -> do
    set
      topic
      [ TopicsRepliesCount +=. (val 1)
      , TopicsLastPoster =. (val $ Just username)
      , TopicsLastPostId =. (val $ Just pid)
      , TopicsLastPost =. (val $ Just now)
      ]
    where_ (topic ^. TopicsId ==. val tid)

selectTopicForumNameByPosterId :: Key Users -> DB [(Value Text, Entity Topics)]
selectTopicForumNameByPosterId userid = do
  select $
    from $ \(forum `InnerJoin` topic `InnerJoin` user) -> do
      on (topic ^. TopicsPoster ==. user ^. UsersUsername)
      on (topic ^. TopicsForumId ==. forum ^. ForumsId)
      where_ (user ^. UsersId ==. val userid)
      orderBy [asc (topic ^. TopicsLastPost)]
      return (forum ^. ForumsName, topic)
