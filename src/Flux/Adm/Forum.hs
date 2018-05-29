{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Flux.Adm.Forum where

import           Import             hiding (Value)

import           Database.Esqueleto

import           DBOp.CRUDForum

createForum :: Grouping -> Key Categories -> Text -> Maybe Text -> Handler ()
createForum Administrator cid name desc =
  liftHandler $ runDB $ insertForum cid name desc
createForum _ _ _ _ =
  permissionDenied "You're not allowed to do this (create forum)."

getForumsAndItsCategory :: Handler [(Text, [(Text, Key Forums)])]
getForumsAndItsCategory = do
  something <- liftHandler $ runDB $ selectAllForumsAndCategoryName
  return $ map (\(cname, mts, mks) -> (cname, spread mts mks)) something
  where
    spread (Just ts) (Just ks) = zip ts ks
    spread _ _                 = []

deleteForums :: Grouping -> [Text] -> Handler ()
deleteForums Administrator fids =
  liftHandler $
  runDB $ forM_ fids (deleteForumById . toSqlKey . forceTextToInt64)
deleteForums _ _ = permissionDenied "You're not allowed to do this (delete forum)"
