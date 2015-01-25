module Handler.Rerun where

import Prelude (head)
import Import
import Control.Distributed.Process.Node
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Hive.Types  (Entry (..), Ticket (..))
import Hive.Client (getHistory, solveRequest)

getRerunR :: Int -> Handler Html
getRerunR ticketNr = do
  yesod <- getYesod
  extra <- getExtra

  let gw   = honey yesod
      ip   = extraMasterHost extra
      port = extraMasterPort extra

  mvar <- liftIO newEmptyMVar
  liftIO $ runProcess gw $ getHistory ip port ticketNr ticketNr mvar
  entry <- liftIO $ takeMVar mvar

  case null entry of
    True  -> redirect (TicketR ticketNr)
    False -> do
      let entry'  = head entry
      newTicketMvar <- liftIO newEmptyMVar
      liftIO $ runProcess gw $ solveRequest ip port (problem entry') newTicketMvar
      newTicketNr <- liftIO $ takeMVar newTicketMvar
      redirect (TicketR . unTicket $ newTicketNr)