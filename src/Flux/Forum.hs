{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Forum where

import           Import             hiding (Value)

import           Database.Esqueleto

import           DBOp.CRUDForum
import           DBOp.CRUDTopic
import           DBOp.CRUDPost

getForumsInformation ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Key Forums
  -> m (Entity Forums)
getForumsInformation fid = do
  forum <- liftHandler $ runDB $ selectForumById fid
  case forum of
    [x] -> return x
    _   -> notFound

getTopicsInForum ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Key Forums
  -> Int64
  -> m [Entity Topics]
getTopicsInForum fid page | page < 0 = invalidArgs ["Yo! You can't look at negative value!"]
getTopicsInForum fid page = liftHandler $ runDB $ selectTopicsByForumIdPage fid page

createTopicByPosting ::
     ( BaseBackend (YesodPersistBackend (HandlerSite m)) ~ SqlBackend
     , PersistStoreWrite (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Key Forums
  -> Key Users
  -> Text
  -> Text
  -> Text
  -> m (Key Topics)
createTopicByPosting fid userid username subject content = do
  now <- liftIO getCurrentTime
  tid <- liftHandler $ runDB $ insertTopic fid username subject
  pid <- liftHandler $ runDB $ insertPost tid 1 username userid content
  return tid

