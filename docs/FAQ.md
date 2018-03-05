# FAQ

### Is this library intended to replace [`async`](https://hackage.haskell.org/package/async)?

**No**, `async` is a fabulous library that allows Applicative composition of
small asynchronous sub-routines into bigger ones and link errors between them.
`async` fits the bill perfectly for small operations that happen concurrently;
on the other hand, `async` is not ideal to supervise long living threads that
need to be restarted in case of failure. This library attempts to fill that gap
by providing APIs that have proven to be successful in the Erlang ecosystem.

### Is this library intended to replace [`distributed-process`](https://hackage.haskell.org/package/distributed-process)?

This library may replace `distributed-process` if the only reason you are using
that library is to get _actor style_ supervision on your threads. In the other
hand if you care to execute `IO ()` sub-routines in different machines, this
library is not and will never be a replacement for `distributed-process`.

### Is this an actor library?

No, while actor systems impose specific inter-process communication semantics to
your application, this library allows developers to use whatever communication
strategies a user sees appropiate (like you would normally have using `forkIO`).
`capataz` only guarantees better reliablity guarantees through adding
supervision semantics to long living `IO ()` sub-routines that are executed
concurrently.
