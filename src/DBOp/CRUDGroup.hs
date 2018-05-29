{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDGroup where

import           Import             hiding (Value, groupBy, on, update, (+=.),
                                     (=.), (==.))

import           Database.Esqueleto

selectGroupByGrouping :: Grouping -> DB [Entity Groups]
selectGroupByGrouping groupname = do
  select $
    from $ \group -> do
      where_ (group ^. GroupsGrouping ==. val groupname)
      limit 1
      return group

selectGroupByUsername ::
     Text -> DB [(Value (Key Users), Value (Key Groups), Value Grouping)]
selectGroupByUsername username = do
  select $
    from $ \(user, group) -> do
      where_
        (user ^. UsersGroupId ==. group ^. GroupsId
         &&. user ^. UsersUsername ==. val username)
      limit 1
      return (user ^. UsersId, group ^. GroupsId, group ^. GroupsGrouping)

selectGroupByUserId ::
     Key Users -> DB [(Value (Key Users), Value (Key Groups), Value Grouping)]
selectGroupByUserId userid = do
  select $
    from $ \(user, group) -> do
      where_
        (user ^. UsersGroupId ==. group ^. GroupsId
         &&. user ^. UsersId ==. val userid)
      limit 1
      return (user ^. UsersId, group ^. GroupsId, group ^. GroupsGrouping)

selectAllGroups :: DB [Entity Groups]
selectAllGroups = do
  select $
    from $ \group -> do
      orderBy [asc (group ^. GroupsGrouping)]
      return group

selectGroupByGroupId :: Key Groups -> DB [Entity Groups]
selectGroupByGroupId gid = do
  select $
    from $ \group -> do
      where_ (group ^. GroupsId ==. val gid)
      limit 1
      return group
