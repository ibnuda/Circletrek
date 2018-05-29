{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDUser where

import           Import             hiding (Value, groupBy, on, update, (+=.),
                                     (=.), (==.), (||.))

import           Database.Esqueleto

import           DBOp.CRUDGroup

insertUser ::
     Key Groups -> Text -> Maybe Text -> Text -> UTCTime -> DB (Key Users)
insertUser usersGroupId usersUsername usersPassword usersEmail usersJoinTime = do
  let usersRepliesPosted = 0
      usersTopicsStarted = 0
  insert $ Users {..}

selectUserById ::
  Key Users
  -> DB [Entity Users]
selectUserById uid = do
  select $
    from $ \user -> do
      where_ (user ^. UsersId ==. val uid)
      limit 1
      return user

selectUserByUsernameOrEmail :: Text -> Text -> DB [Entity Users]
selectUserByUsernameOrEmail username email = do
  select $
    from $ \user -> do
      where_
        (user ^. UsersUsername ==. val username ||. user ^. UsersEmail ==.
         val email)
      limit 1
      return user

updateUserGroupingByUsername :: Text -> Grouping -> DB ()
updateUserGroupingByUsername username grouping = do
  [x] <- selectGroupByGrouping grouping
  update $ \user -> do
    set user [UsersGroupId =. val (entityKey x)]
    where_ (user ^. UsersUsername ==. val username)

qbuilder ::
     (PersistField a, Esqueleto query expr backend, PersistEntity ent)
  => expr (Entity ent)
  -> EntityField ent a
  -> Maybe a
  -> expr (Value Bool)
qbuilder _ _ Nothing           = val True
qbuilder ent accessor (Just v) = ent ^. accessor ==. val v

selectUsersByConditions ::
     Maybe (Key Groups)
  -> Maybe Text
  -> Maybe Text
  -> DB [(Entity Users, Value Grouping)]
selectUsersByConditions mgid musername memail = do
  select $
    from $ \(user, group) -> do
      where_
        (    qbuilder user UsersGroupId mgid
         &&. qbuilder user UsersUsername musername
         &&. qbuilder user UsersEmail memail
         &&. user ^. UsersGroupId ==. group ^. GroupsId)
      orderBy [asc (user ^. UsersUsername)]
      return (user, group ^. GroupsGrouping)

selectUsersBySearchConditions ::
     Maybe Text
  -> Maybe (Key Groups)
  -> SortBy
  -> Bool
  -> DB [(Value Grouping, Entity Users)]
selectUsersBySearchConditions username groupid orderby ascending = do
  select $
    from $ \(user, group) -> do
      where_
        (qbuilder group GroupsId groupid
         &&. qbuilder user UsersUsername username
         &&. user ^. UsersGroupId ==. group ^. GroupsId)
      ordering ascending user orderby
      return (group ^. GroupsGrouping, user)

ordering ::
     Esqueleto query expr backend
  => Bool
  -> expr (Entity Users)
  -> SortBy
  -> query ()
ordering b user Username   = orderBy [(chooseAscension b) (user ^. UsersUsername)]
ordering b user Registered = orderBy [(chooseAscension b) (user ^. UsersJoinTime)]
ordering b user PostCount  = orderBy [(chooseAscension b) (user ^. UsersRepliesPosted)]

chooseAscension ::
     (Esqueleto query expr backend, PersistField a)
  => Bool
  -> expr (Value a)
  -> expr OrderBy
chooseAscension True e  = asc e
chooseAscension False e = desc e

selectAllUsers :: Bool -> DB [(Value Grouping, Entity Users)]
selectAllUsers ascending = do
  let op =
        if ascending
          then asc
          else desc
  select $
    from $ \(user, group) -> do
      where_ (user ^. UsersGroupId ==. group ^. GroupsId)
      orderBy [op (user ^. UsersUsername)]
      return (group ^. GroupsGrouping, user)

updateUserEmail :: Key Users -> Text -> DB ()
updateUserEmail uid email = do
  update $ \user -> do
    set user [UsersEmail =. val email]
    where_ (user ^. UsersId ==. val uid)

updateUserPassword :: Key Users -> Maybe Text -> DB ()
updateUserPassword uid password = do
  update $ \user -> do
    set user [UsersPassword =. val password]
    where_ (user ^. UsersId ==. val uid)

updateUserIncrementPost :: Key Users -> DB ()
updateUserIncrementPost uid = do
  update $ \user -> do
    set user [UsersRepliesPosted +=. val 1]
    where_ (user ^. UsersId ==. val uid)

updateUserIncrementTopic :: Key Users -> DB ()
updateUserIncrementTopic uid = do
  update $ \user -> do
    set user [UsersTopicsStarted +=. val 1]
    where_ (user ^. UsersId ==. val uid)
