module Handler.Profile where

import Import


import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))



data FileForm = FileForm
    { personPassword :: Int
    }
    deriving Show

getProfileR :: Handler Html
getProfileR  = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getProfileR" :: Text

    (first, user) <- requireAuthPair

    defaultLayout $ do
         setTitle "Profile"
         $(widgetFile "profile")



postProfileR :: Handler Html
postProfileR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    (first, user) <- requireAuthPair
    case result of
           FormSuccess res -> do
            if userPassword user == Just 0
              then do
               redirect $ ChecknewR (personPassword res)
              else do
               redirect $ CheckoldR (personPassword res)
           _ -> do         
               defaultLayout $(widgetFile "transfer")  


getCheckoldR :: Int -> Handler Html
getCheckoldR pass = do 
   (first, user) <- requireAuthPair
   if userPassword user == Just pass
    then do
     redirect $ NewpageR
    else do
     defaultLayout $(widgetFile "wrongpass")



sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> areq intField "Password " Nothing

