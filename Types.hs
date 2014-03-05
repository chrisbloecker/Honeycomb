module Types
  where

import Text.Show (Show)
import Data.Eq   (Eq)
import Data.Text (Text)

import Control.Distributed.Process.Node (LocalNode)

-------------------------------------------------------------------------------

type HivePortal = LocalNode

newtype ProblemInput = ProblemInput { unProblemInput :: Text } deriving (Eq, Show)