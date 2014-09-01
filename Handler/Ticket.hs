module Handler.Ticket where

import Prelude (head)
import Import
import Control.Distributed.Process.Node
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Hive.Types  (Problem (..), Solution (..), Entry (..))
import Hive.Client (getHistory)

getTicketR :: Int -> Handler Html
getTicketR ticketNr = do
  mvar  <- liftIO newEmptyMVar
  yesod <- getYesod
  extra <- getExtra
  liftIO $ runProcess (honey yesod) $ getHistory (extraMasterHost extra) (extraMasterPort extra) ticketNr ticketNr mvar
  entry <- liftIO $ takeMVar mvar
  case null entry of
    True  -> defaultLayout [whamlet| <center><h2>Ticket ##{show ticketNr} does not exist! |]
    False -> do
      let p         = problem . head $ entry
      let mSolution = solution . head $ entry
      defaultLayout
        [whamlet|
          <center>
            <h2>Ticket ##{show ticketNr}
          <h3>Problem
          <h4>Type
          <pre>
            #{show $ problemType p}
          <h4>Instance
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
        |]