{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module DBOp.CRUDBan where

import           Import             hiding (Value, groupBy, on, update, (+=.),
                                     (=.), (==.), (||.))

import           Database.Esqueleto

insertBan :: Text -> Maybe Text -> Maybe Text -> Key Users -> DB ()
insertBan bannedsUsername bannedsIp bannedsMessage bannedsExecutor =
  let bannedsStillInEffect = True
  in insert_ Banneds {..}

selectAllBanneds :: DB [(Value (Key Users), Entity Banneds, Value Text)]
selectAllBanneds = do
  select $
    from $ \(user, banned, exec) -> do
      where_
        (user ^. UsersUsername ==. banned ^. BannedsUsername
         &&. banned ^. BannedsExecutor ==. exec ^. UsersId
         &&. banned ^. BannedsStillInEffect ==. val True)
      return (user ^. UsersId, banned, exec ^. UsersUsername)

updateBan :: Text -> Maybe Text -> Maybe Text -> Key Users -> Bool -> DB ()
updateBan username ip message exec status = do
  update $ \banned -> do
    set
      banned
      [ BannedsIp =. val ip
      , BannedsMessage =. val message
      , BannedsExecutor =. val exec
      , BannedsStillInEffect =. val status
      ]
    where_ (banned ^. BannedsUsername ==. val username)
