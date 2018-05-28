{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Concurrent.Capataz.Util (
    -- * Logging utilities
    Logger.buildLogWorkerSpec
  , Logger.buildLogWorkerOptions

    -- * Concurrent utilities
  , Pool.WorkerPoolArgs (..)
  , Pool.buildStealWorkerPoolOptions
  , Pool.buildStealWorkerPoolSpec
  ) where

import qualified Control.Concurrent.Capataz.Util.Logger as Logger
import qualified Control.Concurrent.Capataz.Util.StealWorkerPool as Pool
