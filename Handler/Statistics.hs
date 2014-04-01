module Handler.Statistics where

import Import

import Data.Time   (getCurrentTime, diffUTCTime)
import Widget.Duration (durationWidget)

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Distributed.Process.Node (runProcess)

import Hive.Client (getStatistics)
import Hive.Types  (Statistics (..), seconds)

-------------------------------------------------------------------------------

getStatisticsR :: Handler Html
getStatisticsR = do
  start <- liftIO getCurrentTime
  mvar  <- liftIO newEmptyMVar
  yesod <- getYesod
  extra <- getExtra
  liftIO $ runProcess (honey yesod) $ getStatistics (extraQueenHost extra) (extraQueenPort extra) mvar (seconds 5)
  stats <- liftIO $ takeMVar mvar
  end   <- liftIO getCurrentTime
  let duration = diffUTCTime end start
  defaultLayout $ do
    setTitle "Statistics"
    statisticsWidget stats >> durationWidget duration

statisticsWidget :: Statistics -> Widget
statisticsWidget (Statistics stats) =
  [whamlet|Currently we have to following CPUs attached:
           <pre>
             $forall stat <- stats
               #{show stat}<br>
  |]