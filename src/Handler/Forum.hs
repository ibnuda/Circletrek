{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Forum where

import           Import

import           Data.Time.LocalTime
import           Database.Esqueleto

import           Flux.Forum
import           Handler.Topic

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
  (uid, name, group) <- allowedToPost
  forum <- getForumsInformation (toSqlKey fid)
  topics <- getTopicsInForum (toSqlKey fid ) page
  (wid, enct) <- generateFormPost createTopicForm
  defaultLayout $ do
    setTitle "Index"
    $(widgetFile "forum")

postForumR :: Int64 -> Handler Html
postForumR fid = do
  (uid, name, group) <- allowedToPost
  lock <- lookupPostParam "lock-topic"
  unlock <- lookupPostParam "unlock-topic"
  create <- lookupPostParam "create-topic"
  topicids <- lookupPostParams "topic-id"
  case (lock, unlock, create) of
    (Just _, Nothing, Nothing) -> do
      forM_ topicids $ lockUnlockTopic True group
      redirect $ ForumR fid
    (Nothing, Just _, Nothing) -> do
      forM_ topicids $ lockUnlockTopic False group
      redirect $ ForumR fid
    (Nothing, Nothing, Just _) -> do
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
          redirect $ TopicPageR (fromSqlKey tid) 1
        _ -> invalidArgs ["Come on..."]
    _ -> invalidArgs ["Make up your mind!"]
