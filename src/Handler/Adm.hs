{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Adm where

import           Import

getAdmR :: Handler Html
getAdmR = do
  (uid, name, group) <- allowedToMod
  defaultLayout $(widgetFile "adm")
