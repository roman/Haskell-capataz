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
    WorkerPoolTimeout (..)
  , WorkerPoolArgs (..)
  , buildStealWorkerPoolOptions
  , buildStealWorkerPoolSpec
  )
  where

import RIO
import Control.Concurrent.Capataz

-- | Exception that gets thrown when worker is not able to finish
--   after specified period of time
--
-- @since 0.2.1.0
data WorkerPoolTimeout a =
  WorkerPoolTimeout
  { poolWorkerId      :: !WorkerId
  , poolWorkerName    :: !Text
  , poolWorkerPayload :: !a
  }
  deriving (Show)

instance (Typeable a, Show a) => Exception (WorkerPoolTimeout a)

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
      -- | Maximum amount of time a request processing can take
    , poolWorkerTimeoutMicros :: !Int
    }
  deriving (Generic)

-- | Sub-routine that distributes work to each pool worker
runWorkManager :: Monad m => m a -> m (a -> m ()) -> m ()
runWorkManager pullJob acceptWorker = forever $ do
  sendToWorker <- acceptWorker
  workToDo <- pullJob
  sendToWorker workToDo

-- | Sub-routine that requests new work and does it
runPoolWorker
  :: (MonadUnliftIO m, MonadIO m, MonadThrow m, Typeable a, Show a)
  => ((a -> m ()) -> m ())
  -> Text
  -> WorkerPoolArgs m a
  -> WorkerId
  -> m ()
runPoolWorker requestWork poolWorkerName poolArgs poolWorkerId = do
  let
    WorkerPoolArgs {
        poolWorkerAction
      , poolWorkerTimeoutMicros
      } = poolArgs

  reqBox <- newEmptyMVar
  forever $ do
    requestWork (putMVar reqBox)
    poolWorkerPayload <- takeMVar reqBox

    result <-
      timeout poolWorkerTimeoutMicros
        $ poolWorkerAction poolWorkerId poolWorkerPayload

    case result of
      Nothing ->
        throwM $ WorkerPoolTimeout {
          poolWorkerId
          , poolWorkerName
          , poolWorkerPayload
          }

      Just _ ->
        return ()

-- | Transforms a PoolWorkerSpec into a list of WorkerSpec
buildWorkersFromPoolSpec
  :: (MonadUnliftIO m, MonadIO m, MonadThrow m, Typeable a, Show a)
  => ((a -> m ()) -> m ())
  -> WorkerPoolArgs m a
  -> [ProcessSpec m]
buildWorkersFromPoolSpec requestWork poolArgs =
  let
    WorkerPoolArgs {
        poolWorkerNamePrefix
      , poolWorkerCount
      , poolWorkerOptions
      } = poolArgs
  in
    [ workerSpec1 workerName
                  (runPoolWorker requestWork workerName poolArgs)
                  poolWorkerOptions
    | i <- [1..poolWorkerCount]
    , let workerName = poolWorkerNamePrefix <> "-" <> tshow i
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
--     , poolPullNewWork       = readFromSocket
--     , poolWorkerNamePrefix  = atomically (readTBQueue incomingWorkFromSocket)
--     , poolWorkerCount       = 10
--     , poolWorkerAction      = processWorkFromSocket
--     , poolWorkerOptions     = set workerRestartStrategyL Permanent
--     , poolWorkerTimeoutMicros = 3000100
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
-- __NOTE__ The result of this function must be used as an argument to 'forkSupervisor'
--
-- @since 0.2.1.0
buildStealWorkerPoolOptions
  :: (MonadIO m1, MonadIO m, MonadUnliftIO m, MonadThrow m, Typeable a, Show a)
  => WorkerPoolArgs m a  -- ^ arguments for the worker pool
  -> m1 (SupervisorOptions m)
buildStealWorkerPoolOptions poolArgs = do
  let WorkerPoolArgs {
          poolSupervisorName
        , poolSupervisorOptions
        , poolPullNewWork
        } = poolArgs

  workQueue <- newTBQueueIO (poolWorkerCount poolArgs)
  let
    requestWork  = atomically . writeTBQueue workQueue
    acceptWorker = atomically (readTBQueue workQueue)

    workerSpecList =
      buildWorkersFromPoolSpec requestWork poolArgs

    workerPoolSupervisor =
      supervisorSpec "pool-supervisor"
                     (set supervisorProcessSpecListL workerSpecList
                      . poolSupervisorOptions)

    workManagerSpec =
      workerSpec "work-manager"
                 (runWorkManager poolPullNewWork acceptWorker)
                 (set workerRestartStrategyL Permanent)

    rootSupervisorOptions =
      buildSupervisorOptions poolSupervisorName
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
--     , poolPullNewWork       = readFromSocket
--     , poolWorkerNamePrefix  = atomically (readTBQueue incomingWorkFromSocket)
--     , poolWorkerCount       = 10
--     , poolWorkerAction      = processWorkFromSocket
--     , poolWorkerOptions     = set workerRestartStrategyL Permanent
--     , poolWorkerTimeoutMicros = 3000100
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
-- __NOTE__ The result of this function must be used on the 'setProcessSpecList' of
-- another supervisor.
--
-- @since 0.2.1.0
buildStealWorkerPoolSpec
  :: (MonadIO m1, MonadIO m, MonadUnliftIO m, MonadThrow m, Typeable a, Show a)
  => WorkerPoolArgs m a  -- ^ spec for workers in the pool
  -> m1 (ProcessSpec m)
buildStealWorkerPoolSpec poolArgs = do
  let WorkerPoolArgs { poolSupervisorName } = poolArgs
  options <- buildStealWorkerPoolOptions poolArgs
  return $ supervisorSpec poolSupervisorName (const options)
