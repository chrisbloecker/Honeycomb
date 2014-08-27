module Handler.Ticket where

import Prelude (head)
import Import
import Control.Distributed.Process.Node
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Hive.Types  (Problem (..), Instance (..), Solution (..), Entry (..), Ticket (..))
import Hive.Client (getHistory)

getTicketR :: Int -> Handler Html
getTicketR ticketNr = do
  mvar  <- liftIO newEmptyMVar
  yesod <- getYesod
  extra <- getExtra
  liftIO $ runProcess (honey yesod) $ getHistory (extraMasterHost extra) (extraMasterPort extra) ticketNr ticketNr mvar
  entry <- liftIO $ takeMVar mvar
  let t = ticket . head $ entry
  let p = problem . head $ entry
  let s = solution . head $ entry
  defaultLayout
    [whamlet|
      <center>
        <h2>Ticket #{show ticketNr}
      <h3>Problem
      <h4>Type
      <pre>
        #{show $ problemType p}
      <h4>Instance
      <pre>
        #{unInstance $ inst p}
      <h3>Solution
      <pre>
        $maybe sol <- s
          #{unSolution sol}
        $nothing
          "Not solved yet."
    |]