{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDForum where

import           Import                        hiding (Value, groupBy, on,
                                                update, (+=.), (=.), (==.))

import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL

insertForum :: Key Categories -> Text -> Maybe Text -> DB ()
insertForum cid name desc = do
  insert_ $ Forums cid name desc 0 0 Nothing Nothing Nothing

selectAllForumsAndCategoryName :: DB [(Text, Maybe [Text], Maybe [Key Forums])]
selectAllForumsAndCategoryName = do
  catnameandforums <-
    select $
    from $ \(category, forum) -> do
      where_ (category ^. CategoriesId ==. forum ^. ForumsCategoryId)
      groupBy (category ^. CategoriesName)
      return
        ( category ^. CategoriesName
        , arrayAgg (forum ^. ForumsName)
        , arrayAgg (forum ^. ForumsId))
  return $
    map (\(a, b, c) -> (unValue a, unValue b, unValue c)) catnameandforums

deleteForumById ::
  Key Forums
  -> DB ()
deleteForumById fid = do
  forums <- select $ from $ \forum -> do
    where_ (forum ^. ForumsId ==. val fid)
    return forum
  forM_ forums (deleteCascade . entityKey)

selectTopicsByForumIdPage :: Key Forums -> Int64 -> DB [Entity Topics]
selectTopicsByForumIdPage fid page = do
  select $
    from $ \topic -> do
      where_ (topic ^. TopicsForumId ==. val fid)
      offset ((page - 1) * 25)
      limit 25
      return topic

selectForumById :: Key Forums -> DB [Entity Forums]
selectForumById fid = do
  select $ from $ \forum -> do
    where_ (forum ^. ForumsId ==. val fid)
    limit 1
    return forum

updateForumIncrementReplyAndLasts ::
     Key Forums -> Text -> Key Posts -> UTCTime -> DB ()
updateForumIncrementReplyAndLasts fid username pid last = do
  update $ \forum -> do
    set
      forum
      [ ForumsRepliesCount +=. (val 1)
      , ForumsLastPoster =. (val $ Just username)
      , ForumsLastPostId =. (val $ Just pid)
      , ForumsLastPost =. (val $ Just last)
      ]
    where_ (forum ^. ForumsId ==. val fid)

updateForumIncrementTopic :: Key Forums -> DB ()
updateForumIncrementTopic fid = do
  update $ \forum -> do
    set forum [ForumsTopicsCount +=. (val 1)]
    where_ (forum ^. ForumsId ==. val fid)
