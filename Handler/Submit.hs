module Handler.Submit
  where

import Data.Text.Lazy (fromStrict)

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Distributed.Process.Node hiding (newLocalNode)

import Import
import Types

import Hive.Types  (Problem (..), ProblemType (..), Instance (..))
import Hive.Client (solveRequest)

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
    FormSuccess (ProblemInput p) -> do
      mvar     <- liftIO newEmptyMVar
      yesod    <- getYesod
      liftIO $ runProcess (honey yesod) $ solveRequest (comb yesod) (Problem TSP (Instance . fromStrict $ p)) mvar 2000000
      solution <- liftIO $ takeMVar mvar
      defaultLayout $ do
        setTitle "Solution found!"
        [whamlet|
          <h2>The bees have found a solution for your problem, it is:
          <pre>#{show solution}
        |]
    _ -> redirect SubmitR

problemSubmitForm :: Form ProblemInput
problemSubmitForm = renderDivs $ ProblemInput
  <$> areq textField "Problem: " Nothing