module Handler.History where

import Import
import Control.Arrow ((&&&))
import Control.Distributed.Process.Node
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Hive.Types  (Entry (..), Problem (..), Ticket (..), diffTime)
import Hive.Client (getHistory, clearHistory, getLatestTicket)
import Data.Maybe  (isJust)
import Data.Aeson  (encode)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Yesod.Core.Json

getLatestHistoryR :: Handler Html
getLatestHistoryR = do
  mvar  <- liftIO newEmptyMVar
  yesod <- getYesod
  extra <- getExtra
  liftIO $ runProcess (honey yesod) $ getLatestTicket (extraMasterHost extra) (extraMasterPort extra) mvar
  latestTicket <- liftIO $ takeMVar mvar
  let ticketNr = unTicket latestTicket
  redirect $ HistoryR (max 0 (ticketNr - 30)) ticketNr

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
            <th>
              <b>Rerun
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
              <td>
                <a href=@{RerunR (unTicket $ ticket entry)}>Rerun
        <br>
        #{show from'}-#{show to}
    |]

toDuration :: Entry -> String
toDuration entry =
  case endTime entry of
    Nothing -> "---"
    Just eT -> show $ diffTime (startTime entry) eT

getClearHistoryR :: Handler Html
getClearHistoryR = do
  defaultLayout
    [whamlet|
      <h2>Are you sure you want to clear the history?
      <form method=post action=@{ClearHistoryR}>
        <button>Yes
        <a href=@{HomeR}>No
    |]

postClearHistoryR :: Handler Html
postClearHistoryR = do
  yesod <- getYesod
  extra <- getExtra
  liftIO $ runProcess (honey yesod) $ clearHistory (extraMasterHost extra) (extraMasterPort extra)
  redirect HomeR

getExportHistoryR :: Handler Value
getExportHistoryR = do
  latestTicketNrMvar  <- liftIO newEmptyMVar
  yesod <- getYesod
  extra <- getExtra

  let gw   = honey yesod
      ip   = extraMasterHost extra
      port = extraMasterPort extra

  liftIO $ runProcess gw $ getLatestTicket ip port latestTicketNrMvar
  latestTicket <- liftIO $ takeMVar latestTicketNrMvar
  let latestTicketNr = unTicket latestTicket

  historyMvar <- liftIO newEmptyMVar
  liftIO $ runProcess gw $ getHistory ip port 0 latestTicketNr historyMvar
  history <- liftIO $ takeMVar historyMvar
  let json = decodeUtf8 . encode $ history

  returnJson history