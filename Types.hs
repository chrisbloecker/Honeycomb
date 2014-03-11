module Types
  where

import Text.Show (Show)
import Data.Eq   (Eq)
import Data.Text (Text)
import Data.Time (UTCTime)

import Control.Distributed.Process.Node (LocalNode)

import Hive.Types (ProblemType)

-------------------------------------------------------------------------------

type HivePortal = LocalNode

data ProblemInput = ProblemInput Text ProblemType UTCTime      deriving (Eq, Show)