module Leaderboard.Lens where

import Control.Lens (prism, Prism')
import Database.Beam (Auto (Auto))

_Auto
  :: Prism' (Auto a) a
_Auto =
  prism
    (Auto . Just)
    (\a@(Auto ma) -> maybe (Left a) Right ma)

