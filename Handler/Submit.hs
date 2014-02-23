module Handler.Submit where

import Import
import Types

import Hive.Data   hiding (Problem)
import Hive.Client

import Control.Distributed.Process hiding (Handler)
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
    FormSuccess (Problem p) -> do
      mvar     <- newEmptyMVar :: MVar String
      context  <- liftIO $ initializeBackend "localhost" "52025" initRemoteTable
      node     <- liftIO $ newLocalNode context
      solution <- liftIO $ runProcess node $ solveRequest context TSP "nodes: 1, dima: [1]"
      [whamlet|
        solution
      |]
    _ -> redirect SubmitR

problemSubmitForm :: Form Problem
problemSubmitForm = renderDivs $ Problem
  <$> areq textField "Problem: " Nothing