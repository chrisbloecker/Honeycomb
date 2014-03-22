{-# LANGUAGE RecordWildCards #-}

module Handler.Submit
  where

import Data.Time      (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Text      (pack)
import Data.Text.Lazy (fromStrict)

import Control.Arrow ((&&&))

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Distributed.Process.Node hiding (newLocalNode)

import Import

import Hive.Types  (Problem (..), ProblemType (..), Instance (..), Solution (..))
import Hive.Client (solveRequest)

-------------------------------------------------------------------------------

data ProblemInput = ProblemInput Textarea ProblemType UTCTime      deriving (Eq, Show)

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
    FormSuccess (ProblemInput problemInstance problemType timestamp) -> do
      mvar     <- liftIO newEmptyMVar
      yesod    <- getYesod
      liftIO $ runProcess (honey yesod) $ solveRequest (comb yesod) (Problem problemType (Instance . fromStrict . unTextarea $ problemInstance)) mvar 15000000
      solution <- liftIO $ takeMVar mvar
      now      <- liftIO getCurrentTime
      let duration = diffUTCTime now timestamp
      defaultLayout $ solutionWidget solution >> durationWidget duration
    _ -> redirect SubmitR

problemSubmitForm :: Form ProblemInput
problemSubmitForm = renderDivs $ ProblemInput
  <$> areq textareaField "Problem: " Nothing
  <*> areq (selectFieldList problemTypes) "Type: " Nothing
  <*> lift (liftIO getCurrentTime)
    where
      problemTypes :: [(Text, ProblemType)]
      problemTypes = map (pack . show &&& id) [minBound..maxBound]

solutionWidget :: Solution -> Widget
solutionWidget (Solution {..}) = do
  setTitle "Solution found!"
  [whamlet|
    <h2>Solution found!
    The bees have found a solution for your problem, it is:
    <pre>#{show unSolution}
    With the value:
    <pre>#{show unValue}
  |]
solutionWidget reason = do
  setTitle "No solution found :("
  [whamlet|
    <h2>No solution found :(
    Sorry, we couldn't find any solution. The reason is:
    <pre>#{show reason}
  |]

durationWidget :: NominalDiffTime -> Widget
durationWidget duration = [whamlet|This calculation took #{show duration}.|]