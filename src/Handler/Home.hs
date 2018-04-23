{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import           Import

getHomeR :: Handler Html
getHomeR =
  defaultLayout $ do
    setTitle "Nice"
    [whamlet|
      <h4> Nice.
    |]
