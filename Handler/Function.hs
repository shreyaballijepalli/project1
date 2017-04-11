module Handler.Function where

import Import


import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))


getChecknewR :: Int -> Handler()
getChecknewR pass = do 
   (first, user) <- requireAuthPair
   runDB $ update first [ UserPassword =.Just pass]
   redirect $ NewpageR


