module Handler.Feedback where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getFeedbackR :: Handler Html
getFeedbackR = do
   defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Feedback!"
        $(widgetFile "feedback")

postFeedbackR :: Handler Html
postFeedbackR = do
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Feedback!"
        $(widgetFile "feedback")



commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
