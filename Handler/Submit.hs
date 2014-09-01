{-# LANGUAGE RecordWildCards #-}

module Handler.Submit
  where

import Data.Time      (UTCTime, getCurrentTime, diffUTCTime)
import Data.Text      (pack)
import Data.Text.Lazy (fromStrict)

import Control.Arrow ((&&&))

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Distributed.Process.Node hiding (newLocalNode)

import Import

import Hive.Types  (Problem (..), ProblemType (..), Ticket (unTicket))
import Hive.Client (solveRequest)

import Widget.Duration (durationWidget)

-------------------------------------------------------------------------------

data ProblemInput = ProblemInput ProblemType Textarea UTCTime  deriving (Eq, Show)

-------------------------------------------------------------------------------

getSubmitR :: Handler Html
getSubmitR = do
  (widget, enctype) <- generateFormPost problemSubmitForm
  defaultLayout $ do
    setTitle "Submit your problem!"
    [whamlet|
      <h2>Submit your problem!
      <form method=post action=@{SubmitR} enctype=#{enctype}>
        ^{widget}
        <button>Submit
    |]

postSubmitR :: Handler Html
postSubmitR = do
  ((result, _widget), _enctype) <- runFormPost problemSubmitForm
  case result of
    FormSuccess (ProblemInput problemType problemInstance timestamp) -> do
      mvar     <- liftIO newEmptyMVar
      yesod    <- getYesod
      extra    <- getExtra
      liftIO $ runProcess (honey yesod) $ solveRequest (extraMasterHost extra) (extraMasterPort extra) (Problem problemType (fromStrict . unTextarea $ problemInstance)) mvar
      ticket   <- liftIO $ takeMVar mvar
      now      <- liftIO getCurrentTime
      let duration = diffUTCTime now timestamp
      defaultLayout $ ticketWidget ticket >> durationWidget duration
      redirect (TicketR . unTicket $ ticket)
    _ -> redirect SubmitR

problemSubmitForm :: Form ProblemInput
problemSubmitForm = renderDivs $ ProblemInput
  <$> areq (selectFieldList problemTypes) "Problem Type: " Nothing
  <*> areq textareaField "Problem Instance: " Nothing
  <*> lift (liftIO getCurrentTime)
    where
      problemTypes :: [(Text, ProblemType)]
      problemTypes = map (pack . show &&& id) [minBound..maxBound]

ticketWidget :: Ticket -> Widget
ticketWidget ticket = do
  let ticketNumber = unTicket ticket
  setTitle "Ticket received!"
  [whamlet|
    <h2>Ticket received!
    Ticket received:
    <pre>#{show ticketNumber}
  |]