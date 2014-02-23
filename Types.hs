module Types
  where

import Text.Show
import Data.Eq
import Data.Text

newtype Problem = Problem { unProblem :: Text } deriving (Eq, Show)