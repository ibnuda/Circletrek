{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Adm.Category where

import           Import

import           DBOp.CRUDCategory

createCategory :: Grouping -> Text -> Handler (Key Categories)
createCategory Administrator catname =
  liftHandler $ runDB $ insertCategory catname
createCategory _ _ =
  permissionDenied "You're not allowed to do this (category creation)."

getAllCategories :: Handler [Entity Categories]
getAllCategories = liftHandler $ runDB $ selectAllCategory

deleteCategoryCascade :: Grouping -> Key Categories -> Handler ()
deleteCategoryCascade Administrator cid = liftHandler $ runDB $ deleteCategory cid
deleteCategoryCascade _ _ =
  permissionDenied "You're not allowed to do this (category deletion)."
