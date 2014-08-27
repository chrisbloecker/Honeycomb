module Handler.History where

import Import
import Control.Distributed.Process.Node
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Hive.Types  (Entry (..), Problem (..), Ticket (..))
import Hive.Client (getHistory)
import Data.Maybe  (isJust)

getHistoryR :: Int -> Int -> Handler Html
getHistoryR from to = do
  mvar    <- liftIO newEmptyMVar
  yesod   <- getYesod
  extra   <- getExtra
  liftIO $ runProcess (honey yesod) $ getHistory (extraMasterHost extra) (extraMasterPort extra) from to mvar
  history <- liftIO $ takeMVar mvar
  defaultLayout $ do
    setTitle "History"
    toWidget [lucius| 
      table, th, td { border: 1px solid black }
      td { text-align: center }
    |]
    [whamlet|
      <center>
        <h2>History
      <center>
        <table>
          <tr>
            <th>
              <b>Ticket
            <th>
              <b>Problem type
            <th>
              <b>Solved
          $forall entry <- history
            <tr>
              <td>
                <a href=@{TicketR (unTicket $ ticket entry)}>#{show $ unTicket $ ticket entry}
              <td>
                #{show $ problemType $ problem entry}
              <td>
                $if isJust (solution entry)
                  Yes
                $else
                  No
        <br>
        #{show from}-#{show to}
    |]