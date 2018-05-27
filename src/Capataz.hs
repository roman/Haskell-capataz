-- | Convinience module that re-exports modules:
--
-- * "Control.Concurrent.Capataz"
-- * "Control.Concurrent.Capataz.Util"
--
--  @since 0.2.0.0
module Capataz
  (
    module X
  ) where

import Control.Concurrent.Capataz      as X
import Control.Concurrent.Capataz.Util as X
