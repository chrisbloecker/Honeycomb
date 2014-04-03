module Widget.Duration
  ( durationWidget
  )
  where

import Import
import Data.Time (NominalDiffTime)

-------------------------------------------------------------------------------

durationWidget :: NominalDiffTime -> Widget
durationWidget duration = [whamlet|This calculation took #{show duration}.|]