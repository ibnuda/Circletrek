{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Miscellaneous where

import           Import

import           DBOp.CRUDGroup

getAllGroups :: Handler [Entity Groups]
getAllGroups = liftHandler $ runDB $ selectAllGroups

getGroup :: Grouping -> Handler (Entity Groups)
getGroup x = do
  a <- liftHandler . runDB . selectGroupByGrouping $ x
  case a of
    [g] -> return g
    _   -> invalidArgs ["Group not found."]

getGroupById :: Key Groups -> Handler (Entity Groups)
getGroupById gid = do
  a <- liftHandler . runDB . selectGroupByGroupId $ gid
  case a of
    [g] -> return g
    _   -> invalidArgs ["Group not found."]
