{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Adm.Ban where

import           Import             hiding (Value)

import           Database.Esqueleto

import           DBOp.CRUDBan
import           DBOp.CRUDGroup
import           DBOp.CRUDUser

getAllBanneds :: Handler [(Value (Key Users), Entity Banneds, Value Text)]
getAllBanneds = liftHandler $ runDB $ selectAllBanneds

banUser ::
     Key Users
  -> p
  -> Grouping
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Handler ()
banUser execid execname execgroup username ip message = do
  gusername <- liftHandler $ runDB $ selectGroupByUsername username
  case gusername of
    [] -> invalidArgs ["There's no user named " <> username]
    x:_ -> do
      let (uid, gid, group) = (\(Value u, Value gi, Value g) -> (u, gi, g)) x
      case (banResult execgroup group, execid == uid) of
        (Right _, False) -> do
          liftHandler $ runDB $ do
            updateUserGroupingByUsername username Banned
            insertBan username ip message execid
        (Right _, True)  -> invalidArgs ["You cannot ban yourself."]
        (Left x, _)      -> invalidArgs [x]

banUserById ::
     Key Users
  -> p
  -> Grouping
  -> Key Users
  -> Maybe Text
  -> Maybe Text
  -> Handler ()
banUserById execid execname execgroup userid ip message = do
  [user] <- liftHandler $ runDB $ selectUserById userid
  banUser execid execname execgroup (usersUsername $ entityVal user) ip message

unbanUser :: Key Users -> Text -> Grouping -> Text -> Handler ()
unbanUser execid execname execgroup username = do
  gusernames <- liftHandler $ runDB $ selectGroupByUsername username
  case gusernames of
    [] -> invalidArgs ["There's no user named " <> username]
    x:_ -> do
      let (uid, gid, group) = (\(Value u, Value gi, Value g) -> (u, gi, g)) x
      case (unbanResult execgroup group, execid == uid) of
        (Right _, False) -> do
          liftHandler $ runDB $ do
            updateUserGroupingByUsername username Member
            updateBan username Nothing Nothing execid False
        (Right _, True)  -> invalidArgs ["You cannot unban yourself."]
        (Left x, _)      -> invalidArgs [x]

unbanResult Administrator Banned = Right ()
unbanResult Moderator Banned     = Right ()
unbanResult Administrator _      = Left "Cannot unban non-banned"
unbanResult Moderator _          = Left "Cannot unban non-banned"
unbanResult _ _                  = Left "You have no right to do so."

banResult Administrator Administrator = Left "Cannot ban an Admin"
banResult Administrator Moderator     = Left "Cannot ban a Mod"
banResult Administrator Member        = Right ()
banResult Administrator Banned        = Left "Cannot ban an already banned user."
banResult Moderator Administrator     = Left "It's above your paygrade."
banResult Moderator Moderator         = Left "Cannot ban a Mod"
banResult Moderator Member            = Right ()
banResult _ _                         = Left "You're not allowed to ban something."
