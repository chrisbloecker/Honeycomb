module Types
  where

import Text.Show
import Data.Eq
import Data.Text

import Control.Distributed.Process.Node

type HivePortal = LocalNode

newtype ProblemInput = ProblemInput { unProblemInput :: Text } deriving (Eq, Show)