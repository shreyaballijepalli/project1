module Handler.Newpage where

import Import hiding (atomically)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

data FileForm = FileForm
    { personId   :: Text
    , personAmount :: Int
    }
    deriving Show

type Account = TMVar Int

newAccount :: Int -> IO Account
newAccount i = newTMVarIO i

takeAccount :: Account -> STM Int
takeAccount account = takeTMVar account

releaseAccount :: Int -> Account -> STM ()
releaseAccount i account = putTMVar account i
{-
putLock :: Account -> STM Int
putLock = atomically $ do
   account1 <- newAccount 10
   lock1 <- takeAccount account1
   return lock1 -}
        
getNewpageR :: Handler Html
getNewpageR  = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getNewpageR" :: Text

    maybeCurrentUserId <- maybeAuthId
    (first, user) <- requireAuthPair
    defaultLayout $ do
          $(widgetFile "newpage")

getFuncR ::  Int -> Text -> Handler()
getFuncR amount id = do 
   (first, user) <- requireAuthPair
   --transfersuccess 
   runDB $ update first [ UserAmount -=. Just amount ]
   runDB $ updateWhere [UserIdent ==. id] [UserAmount +=. Just amount]



transfersuccess :: Handler Html
transfersuccess = do
      defaultLayout $ do
          $(widgetFile "transfer")

   
postNewpageR :: Handler Html
postNewpageR  = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    (first, user) <- requireAuthPair
    case result of
            FormSuccess res -> do
                 if userAmount user >= Just (personAmount res)
                  then do  
                     redirect $ FuncR (personAmount res) (personId res) 	
                  else do
                     defaultLayout $(widgetFile "transferfail")
            _ -> do         
               defaultLayout $(widgetFile "transfer")   
        


sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> areq textField "TransferId  " Nothing 
    <*> areq intField "Amount  " Nothing 

 
