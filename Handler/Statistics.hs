module Handler.Statistics where

import Import

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Distributed.Process.Node (runProcess)

import Hive.Client (getStatistics)
import Hive.Types  (Statistics (..))

-------------------------------------------------------------------------------

getStatisticsR :: Handler Html
getStatisticsR = do
  mvar  <- liftIO newEmptyMVar
  yesod <- getYesod
  liftIO $ runProcess (honey yesod) $ getStatistics (comb yesod) mvar 5000000
  stats <- liftIO $ takeMVar mvar
  defaultLayout $ do
    setTitle "Statistics"
    statisticsWidget stats

statisticsWidget :: Statistics -> Widget
statisticsWidget (Statistics stats) =
  [whamlet|Currently we have to following CPUs attached:
           <pre>
             $forall stat <- stats
               #{show stat}<br>
  |]