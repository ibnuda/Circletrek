{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import           Foundation
import           Yesod.Core

getHomeR :: Handler Html
getHomeR =
  defaultLayout $ do
    setTitle "Nice"
    [whamlet|
      <h4> Nice.
    |]
