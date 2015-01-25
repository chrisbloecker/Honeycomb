module Handler.Ticket where

import Prelude (head)
import Import
import Control.Distributed.Process.Node
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Hive.Types  (Problem (..), Solution (..), Entry (..), diffTime)
import Hive.Client (getHistory)

getTicketR :: Int -> Handler Html
getTicketR ticketNr = do
  mvar  <- liftIO newEmptyMVar
  yesod <- getYesod
  extra <- getExtra
  liftIO $ runProcess (honey yesod) $ getHistory (extraMasterHost extra) (extraMasterPort extra) ticketNr ticketNr mvar
  entry <- liftIO $ takeMVar mvar
  case null entry of
    True  -> defaultLayout [whamlet| <center><h2>Ticket ##{show ticketNr} does not exist!|]
    False -> do
      let p         = problem  . head $ entry
      let mSolution = solution . head $ entry
      let mEndTime  = endTime  . head $ entry
      let duration  = case mEndTime of
                        Nothing      -> ""
                        Just endTime -> show $ diffTime (startTime . head $ entry) endTime
      defaultLayout
        [whamlet|
          <center>
            <h2>Ticket ##{show ticketNr}
          <h3>Problem
          <h4>Type
          <pre>
            #{show $ problemType p}
          <h4>Instance
          <h6>
            <a href=@{RerunR ticketNr}>Rerun input
          <pre>
            #{problemInstance p}
          <h3>Solution
          <pre>
            $maybe solution <- mSolution
              $case solution
                $of Solution sol
                  #{sol}
                $of x
                  #{show x}
            $nothing
              "Not solved yet."
          <pre>
            #{duration}
        |]