{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Offers functions that build a pool of workers that run concurrently in a
-- supervised environment
--
-- @since 0.2.1.0
module Control.Concurrent.Capataz.Util.StealWorkerPool
  (
    buildStealWorkerPoolOptions
  , buildStealWorkerPoolSpec
  )
  where

import RIO
import Control.Concurrent.Capataz

data PoolWorkerSpec m a
  = PoolWorkerSpec
    { poolWorkerNamePrefix  :: !Text -- ^ Prefix of worker's name in the pool (used for telemetry)
    , poolWorkerCount       :: !Int  -- ^ Number of worker threads on the pool
    , poolWorkerAction      :: !(WorkerId -> a -> m ()) -- ^ Sub-routine that does something with a Job
    , poolWorkerOptions     :: !(WorkerOptions m -> WorkerOptions m) -- ^ Supervision options for workers in the pool
    }
  deriving (Generic)

-- | Sub-routine that distributes work to each pool worker
runWorkManager :: Monad m => m a -> m (a -> m ()) -> m ()
runWorkManager pullJob acceptWorker = forever $ do
  sendToWorker <- acceptWorker
  workToDo <- pullJob
  sendToWorker workToDo

-- | Sub-routine that requests new work and does it
runPoolWorker :: MonadIO m => ((a -> m ()) -> m ()) -> (WorkerId -> a -> m ()) -> WorkerId -> m ()
runPoolWorker requestWork workerAction workerId = do
  reqBox <- newEmptyMVar
  forever $ do
    requestWork (putMVar reqBox)
    a <- takeMVar reqBox
    workerAction workerId a

-- | Transforms a PoolWorkerSpec into a list of WorkerSpec
buildWorkersFromPoolSpec :: MonadIO m => ((a -> m ()) -> m ()) -> PoolWorkerSpec m a -> [ProcessSpec m]
buildWorkersFromPoolSpec requestWork workerPoolSpec =
  let
    PoolWorkerSpec {
        poolWorkerNamePrefix
      , poolWorkerCount
      , poolWorkerAction
      , poolWorkerOptions
      } = workerPoolSpec
  in
    [ workerSpec1 (poolWorkerNamePrefix <> "-" <> tshow i)
                  (runPoolWorker requestWork poolWorkerAction)
                  poolWorkerOptions
    | i <- [1..poolWorkerCount]
    ]

-- | This function returns the settings needed to /dynamically/ build a
-- supervision tree that contains:
--
-- 1. A pool of worker threads that process a given job
-- 2. A worker thread that pulls jobs and pushes it to available workers (alias work manager)
-- 3. Two supervisors (one for the pool) and one for the pool supervisor and the work manager
--
-- For the following invokation
--
-- @
-- stealWorkerPoolProcessOptions
--   "my-worker-pool"
--   (set supervisorRestartStraregyL AllForOne)
--   (atomically $ readTBQueue incomingWorkFromSocket)
--   PoolWorkerSpec { poolWorkerNamePrefix = "worker-pool"
--                  , poolWorkerCount      = 10
--                  , poolWorkerAction     = processWorkFromSocket
--                  , poolWorkerOptions    = set workerRestartStrategyL Permanent }
-- @
--
-- When we call 'forkSupervisor', a supervision tree like the following is
-- spawned:
--
-- @
-- my-worker-pool
--   ├── worker-manager
--   └── pool-supervisor
--       │── worker-pool-1
--       │── ...
--       └── worker-pool-10
-- @
--
-- **NOTE** The result of this function must be used as an argument to 'forkSupervisor'
--
-- @since 0.2.1.0
buildStealWorkerPoolOptions
  :: (MonadIO m1, MonadIO m)
  => SupervisorName      -- ^ Name of pool supervisor
  -> (SupervisorOptions m -> SupervisorOptions m) -- ^ Options for the supervisor of the worker pool
  -> m a                 -- ^ sub-routine that pulls new work to get processed (read SQS, read TQueue, etc.)
  -> PoolWorkerSpec m a  -- ^ spec for workers in the pool
  -> m1 (SupervisorOptions m)
buildStealWorkerPoolOptions supName poolSupervisorOptions pullJob workerPoolSpec = do
    workQueue <- newTBQueueIO (poolWorkerCount workerPoolSpec)
    let
      requestWork  = atomically . writeTBQueue workQueue
      acceptWorker = atomically (readTBQueue workQueue)

      workerSpecList =
        buildWorkersFromPoolSpec requestWork workerPoolSpec

      workerPoolSupervisor =
        supervisorSpec "pool-supervisor"
                       (set supervisorProcessSpecListL workerSpecList
                        . poolSupervisorOptions)

      workManagerSpec =
        workerSpec "work-manager"
                   (runWorkManager pullJob acceptWorker)
                   (set workerRestartStrategyL Permanent)

      rootSupervisorOptions =
        buildSupervisorOptions supName (set supervisorProcessSpecListL [workManagerSpec, workerPoolSupervisor])

    return rootSupervisorOptions

-- | This function returns the settings needed to /statically/ build a
-- supervision tree that contains:
--
-- 1. A pool of worker threads that process a given job
-- 2. A worker thread that pulls jobs and pushes it to available workers (alias work manager)
-- 3. Two supervisors (one for the pool) and one for the pool supervisor and the work manager
--
-- For the following invokation
--
-- @
-- buildStealWorkerPoolSpec
--   "my-worker-pool"
--   (set supervisorRestartStraregyL AllForOne)
--   (atomically $ readTBQueue incomingWorkFromSocket)
--   PoolWorkerSpec { poolWorkerNamePrefix = "worker-pool"
--                  , poolWorkerCount      = 10
--                  , poolWorkerAction     = processWorkFromSocket
--                  , poolWorkerOptions    = set workerRestartStrategyL Permanent }
-- @
--
-- A supervision tree like the following is spawned:
--
-- @
-- my-worker-pool
--   ├── worker-manager    <- pulls new jobs and delivers it to worker-pool-[1,10]
--   └── pool-supervisor
--       │── worker-pool-1 <- requests worker-manger for new work, and then executes it
--       │── ...
--       └── worker-pool-10
-- @
--
-- **NOTE** The result of this function must be used on the 'setProcessSpecList' of
-- another supervisor.
--
-- @since 0.2.1.0
buildStealWorkerPoolSpec
  :: (MonadIO m1, MonadIO m)
  => SupervisorName      -- ^ Name of pool supervisor
  -> (SupervisorOptions m -> SupervisorOptions m) -- ^ Options for the supervisor of the worker pool
  -> m a                 -- ^ sub-routine that pulls new work to get processed (read SQS, read TQueue, etc.)
  -> PoolWorkerSpec m a  -- ^ spec for workers in the pool
  -> m1 (ProcessSpec m)
buildStealWorkerPoolSpec supName poolSupervisorOptions pullJob workerPoolSpec = do
  options <- buildStealWorkerPoolOptions supName poolSupervisorOptions pullJob workerPoolSpec
  return $ supervisorSpec supName (const options)
