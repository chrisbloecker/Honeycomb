module Handler.History where

import Import
import Control.Arrow ((&&&))
import Control.Distributed.Process.Node
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Hive.Types  (Entry (..), Problem (..), Ticket (..), diffTime)
import Hive.Client (getHistory)
import Data.Maybe  (isJust)

getHistoryR :: Int -> Int -> Handler Html
getHistoryR from to = do
  mvar    <- liftIO newEmptyMVar
  yesod   <- getYesod
  extra   <- getExtra
  let from' = max 0 from
  liftIO $ runProcess (honey yesod) $ getHistory (extraMasterHost extra) (extraMasterPort extra) from' to mvar
  history <- liftIO $ takeMVar mvar
  let history' = map (id &&& toDuration) history
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
            <th>
              <b>Time
          $forall (entry, duration) <- history'
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
              <td>
                $if isJust (solution entry)
                  #{duration}
        <br>
        #{show from'}-#{show to}
    |]

toDuration :: Entry -> String
toDuration entry =
  case endTime entry of
    Nothing -> "---"
    Just eT -> show $ diffTime (startTime entry) eT