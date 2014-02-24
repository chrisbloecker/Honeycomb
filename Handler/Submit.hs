module Handler.Submit where

import Import
import Types

import Data.Text

import Hive.Data
import Hive.Client

import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node hiding (newLocalNode)

import Control.Concurrent.MVar

getSubmitR :: Handler Html
getSubmitR = do
  (widget, enctype) <- generateFormPost problemSubmitForm
  defaultLayout [whamlet|
                  <h2>Submit a problem
                  <form method=post action=@{SubmitR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                |]

postSubmitR :: Handler Html
postSubmitR = do
  ((result, widget), enctype) <- runFormPost problemSubmitForm
  case result of
    FormSuccess (ProblemInput p) -> do
      mvar     <- liftIO newEmptyMVar
      yesod    <- getYesod
      liftIO $ runProcess (honey yesod) $ solveRequest (comb yesod) TSP (unpack p) mvar 2000000
      solution <- liftIO $ takeMVar mvar
      defaultLayout [whamlet|
                      #{show solution}
                    |]
    _ -> redirect SubmitR

problemSubmitForm :: Form ProblemInput
problemSubmitForm = renderDivs $ ProblemInput
  <$> areq textField "Problem: " Nothing