{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Offers functions that build a pool of workers that run concurrently in a
-- supervised environment
--
-- @since 0.2.1.0
module Control.Concurrent.Capataz.Util.StealWorkerPool
  (
    WorkerPoolArgs (..)
  , buildStealWorkerPoolOptions
  , buildStealWorkerPoolSpec
  )
  where

import Control.Concurrent.Capataz
import RIO

-- | Record help gather arguments to build a pool of workers
--
-- @since 0.2.1.0
data WorkerPoolArgs m a
  = WorkerPoolArgs
    {
      -- | sub-routine that pulls new work to get processed (read SQS, read TQueue, etc.)
      poolPullNewWork       :: !(m a)
      -- | Name of pool supervisor (used for telemetry)
    , poolSupervisorName    :: !SupervisorName
      -- | Options for the supervisor of the worker pool
    , poolSupervisorOptions :: !(SupervisorOptions m -> SupervisorOptions m)
      -- | Prefix of worker's name in the pool (used for telemetry)
    , poolWorkerNamePrefix  :: !Text
      -- | Number of worker threads on the pool
    , poolWorkerCount       :: !Int
      -- | Sub-routine that does something with a pulled job
    , poolWorkerAction      :: !(WorkerId -> a -> m ())
      -- | Supervision options for workers in the pool
    , poolWorkerOptions     :: !(WorkerOptions m -> WorkerOptions m)
    }
  deriving (Generic)

-- | Sub-routine that distributes work to each pool worker
runWorkManager :: Monad m => m a -> m (a -> m ()) -> m ()
runWorkManager pullJob acceptWorker = forever $ do
  sendToWorker <- acceptWorker
  workToDo     <- pullJob
  sendToWorker workToDo

-- | Sub-routine that requests new work and does it
runPoolWorker
  :: MonadIO m
  => ((a -> m ()) -> m ())
  -> (WorkerId -> a -> m ())
  -> WorkerId
  -> m ()
runPoolWorker requestWork workerAction workerId = do
  reqBox <- newEmptyMVar
  forever $ do
    requestWork (putMVar reqBox)
    a <- takeMVar reqBox
    workerAction workerId a

-- | Transforms a PoolWorkerSpec into a list of WorkerSpec
buildWorkersFromPoolSpec
  :: MonadIO m => ((a -> m ()) -> m ()) -> WorkerPoolArgs m a -> [ProcessSpec m]
buildWorkersFromPoolSpec requestWork poolArgs =
  let
    WorkerPoolArgs { poolWorkerNamePrefix, poolWorkerCount, poolWorkerAction, poolWorkerOptions }
      = poolArgs
  in  [ workerSpec1 (poolWorkerNamePrefix <> "-" <> tshow i)
                    (runPoolWorker requestWork poolWorkerAction)
                    poolWorkerOptions
      | i <- [1 .. poolWorkerCount]
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
-- buildStealWorkerPoolOptions
--   WorkerPoolArgs {
--       poolSupervisorName    = "my-worker-pool"
--     , poolSupervisorOptions = set supervisorRestartStraregyL AllForOne
--     , poolPullNewWork       = "worker-pool"
--     , poolWorkerNamePrefix  = atomically (readTBQueue incomingWorkFromSocket)
--     , poolWorkerCount       = 10
--     , poolWorkerAction      = processWorkFromSocket
--     , poolWorkerOptions     = set workerRestartStrategyL Permanent
--     }
--
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
  => WorkerPoolArgs m a  -- ^ arguments for the worker pool
  -> m1 (SupervisorOptions m)
buildStealWorkerPoolOptions poolArgs = do
  let
    WorkerPoolArgs { poolSupervisorName, poolSupervisorOptions, poolPullNewWork }
      = poolArgs

  workQueue <- newTBQueueIO (poolWorkerCount poolArgs)
  let
    requestWork          = atomically . writeTBQueue workQueue
    acceptWorker         = atomically (readTBQueue workQueue)

    workerSpecList       = buildWorkersFromPoolSpec requestWork poolArgs

    workerPoolSupervisor = supervisorSpec
      "pool-supervisor"
      (set supervisorProcessSpecListL workerSpecList . poolSupervisorOptions)

    workManagerSpec = workerSpec
      "work-manager"
      (runWorkManager poolPullNewWork acceptWorker)
      (set workerRestartStrategyL Permanent)

    rootSupervisorOptions = buildSupervisorOptions
      poolSupervisorName
      (set supervisorProcessSpecListL [workManagerSpec, workerPoolSupervisor])

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
--   WorkerPoolArgs {
--       poolSupervisorName    = "my-worker-pool"
--     , poolSupervisorOptions = set supervisorRestartStraregyL AllForOne
--     , poolPullNewWork       = "worker-pool"
--     , poolWorkerNamePrefix  = atomically (readTBQueue incomingWorkFromSocket)
--     , poolWorkerCount       = 10
--     , poolWorkerAction      = processWorkFromSocket
--     , poolWorkerOptions     = set workerRestartStrategyL Permanent
--     }
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
  => WorkerPoolArgs m a  -- ^ spec for workers in the pool
  -> m1 (ProcessSpec m)
buildStealWorkerPoolSpec poolArgs = do
  let WorkerPoolArgs { poolSupervisorName } = poolArgs
  options <- buildStealWorkerPoolOptions poolArgs
  return $ supervisorSpec poolSupervisorName (const options)
