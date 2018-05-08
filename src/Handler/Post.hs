{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Post where

import           Import

import           Data.Time.LocalTime
import           Database.Esqueleto
import           Database.Persist.Sql (fromSqlKey, toSqlKey)

import           Flux.Forum
import           Flux.Post
import           Flux.Topic

getPostR :: Int64 -> Handler Html
getPostR pid = do
  (uid, name, group) <- allowedToPost
  (Entity _ (Posts tid num _ _ _ _)) <- getPostById $ toSqlKey pid
  let page = floor $ (toRational num - 1) / 25 + 1 :: Int64
  redirect $ TopicPageR (fromSqlKey tid) page :#: ("post-" <> show num)

data EditPostForm = EditPostForm
  { editPostFormContent :: Textarea
  } deriving (Show)

editPostForm :: Text -> Form EditPostForm
editPostForm content =
  renderDivs $
  EditPostForm <$> areq textareaField "Post's Content" (Just . Textarea $ content)

getPostEditR :: Int64 -> Handler Html
getPostEditR pid = do
  (uid, name, group) <- allowedToPost
  (Value fname, Value fid, Value tsub, (Entity pid' (Posts tid num name' uid' t content))) <-
    getPostParentInformation $ toSqlKey pid
  (wid, enct) <- generateFormPost $ editPostForm content
  defaultLayout $ do
    $(widgetFile "post")

postPostEditR :: Int64 -> Handler Html
postPostEditR pid = do
  (uid, name, group) <- allowedToPost
  post <- getPostById $ toSqlKey pid
  ((res, _), _) <- runFormPost . editPostForm . postsContent . entityVal $ post
  case res of
    FormSuccess c -> do
      editPostByUidGroupAndContent
        uid
        group
        (toSqlKey pid )
        (postsUserId $ entityVal post)
        (unTextarea $ editPostFormContent c)
      redirect $ PostR pid
    _ -> invalidArgs ["Come on..."]
