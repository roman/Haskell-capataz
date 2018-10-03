# Change log

capataz uses [Semantic Versioning][1].
The change log is available [on GitHub][2].

[1]: http://semver.org/spec/v2.0.0.html
[2]: https://github.com/roman/capataz/releases

## v0.2.1.0

* Export `buildWorkerOptions1` and `workerSpec1` through the public API
* Add `buildStealWorkerPoolSpec` and `buildStealWorkerPoolOptions`, which allows
  you to create a supervision tree for pool of workers

## v0.2.0.0 Bumblebee release

**BREAKING CHANGES**

* Allow the creation of Supervision Trees on Monad Transformer stacks 🎉
* Move to latest stable stackage snapshot (lts-11.10)
* Remove `protolude` in favor of `rio` (closes #9)
* Add convinience module `Capataz`
* Add `Control.Concurrent.Capataz.Util` which provide `ProcessSpec` for normal use-cases
* Add `buildLogWorkerSpec` and `buildLogWorkerOptions` which create a dedicated supervised
  thread that logs messages (sits on top of `rio` logger utilities)
* Add `terminateCapataz` and `terminateCapataz_` , these functions replace the
  re-exported `runTeardown`
* Add re-export of `TeardownResult` from the `teardown` package
* Update the `capataz-simple-example` project to use `typed-process` and `rio`
* Add export for `WorkerName` and `SupervisorName` aliases
* Add new dependency `prettyprinter`
* Add new dependency `pretty-show` to prettify exceptions
* Add `Pretty` instance for `CapatazEvent`
* Add `Display` instance for `CapatazEvent`
* Add `ProcessThreadId` newtype to better render `ThreadId` values
* Add `buildWorkerOptions1` and `workerSpec1` (closes #17)
* Move to CircleCI

## v0.1.0.1

* Bump bounds of `async` dependency

## v0.1.0.0 Who supervises the supervisor?

**BREAKING CHANGES**

* Introduction of the `Process` type which is composed of both `Supervisor` and
  `Worker` types
* Replace `defWorkerSpec` in favor of `workerSpec` and `workerSpecWithDefaults`
  to build static workers
* Replace of `defWorkerOptions` in favor of `buildWorkerOptions` and
  `buildWorkerOptionsWithDefaults` to build dynamic workers
* Replace `terminateWorker` in favor of `terminateProcess`
* Add `supervisorSpec`, `supervisorSpecWithDefaults` to build static supervision
  trees
* Add `forkSupervisor`, `buildSupervisorOptions` and
  `buildSupervisorOptionsWithDefaults` to build dynamic supervision trees
* Replace usage of default records semantics in favor of Lenses
* Add `joinCapatazThread` to avoid providing direct access to async of root
  supervision tree
* Add `getSupervisorProcessId` to access the `ProcessId` of a given `Supervisor`
  record (for dynamic termination)
* Add `getSupervisorAsync` to access the `Async ()` record of a supervisor
  process thread
* Add `getCapatazTeardown` to access the `Teardown` record of the capataz system
* Move `CapatazEvent` records to new module `Control.Concurrent.Capataz.Event`
  to avoid requiring `DuplicateRecordFields` extension on API users
* Remove `WorkerAction` alias as it is used for library development
  documentation
* Add capataz-repo-watcher example to showcase static supervision trees
* Update capataz-simple-example unix-process example
* `forkCapataz` signature now requires name for root supervisor

## v0.0.0.2

* Bump bounds of `tasty` dependency

## v0.0.0.1

* Bump bounds of `tasty` dependency
* Bump bounds of `tasty-hunit` dependency

## v0.0.0.0

* First release of capataz
* Support for supervising simple worker `IO ()` sub-routines
