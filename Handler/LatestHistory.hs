module Handler.LatestHistory where

import Import
import Control.Distributed.Process.Node
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Hive.Types  (Problem (..), Solution (..), Entry (..), unTicket)
import Hive.Client (getLatestTicket)

getLatestHistoryR :: Handler Html
getLatestHistoryR = do
  mvar  <- liftIO newEmptyMVar
  yesod <- getYesod
  extra <- getExtra
  liftIO $ runProcess (honey yesod) $ getLatestTicket (extraMasterHost extra) (extraMasterPort extra) mvar
  latestTicket <- liftIO $ takeMVar mvar
  let ticketNr = unTicket latestTicket
  redirect $ HistoryR (max 0 (ticketNr - 30)) ticketNr