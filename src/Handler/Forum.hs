{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Forum where

import           Import

import           Data.Time.LocalTime
import           Database.Esqueleto

import           Flux.Forum

data CreateTopicForm = CreateTopicForm
  { createTopicFormSubject :: Text
  , createTopicFormContent :: Textarea
  }

createTopicForm :: Form CreateTopicForm
createTopicForm =
  renderDivs $
  CreateTopicForm <$> areq textField "New Subject" Nothing <*>
  areq textareaField "Opening Post" Nothing

getForumR :: Int64 -> Handler Html
getForumR fid = redirect $ ForumPageR fid 1

getForumPageR :: Int64 -> Int64 -> Handler Html
getForumPageR fid page = do
  forum <- getForumsInformation (toSqlKey fid)
  topics <- getTopicsInForum (toSqlKey fid ) page
  (wid, enct) <- generateFormPost createTopicForm
  defaultLayout $ do
    setTitle "Index"
    $(widgetFile "forum")

postForumR :: Int64 -> Handler Html
postForumR fid = do
  (uid, name, group) <- allowedToPost
  ((res, wid), enct) <- runFormPost createTopicForm
  case res of
    FormSuccess r -> do
      tid <-
        createTopicByPosting
          (toSqlKey fid)
          uid
          name
          (createTopicFormSubject r)
          (unTextarea $ createTopicFormContent r)
      redirect $ ForumR fid -- we will back to it later.
    _ -> invalidArgs ["Come on..."]
