# Purpose

This library is intended to be a drop-in replacement to `forkIO`, with the
difference that you can specify well-known strategies on how to deal with errors
gracefully and reliably. A difference with vanilla `forkIO` is that you'll need
to do a bit more of setup specifying supervision rules, and also you need to
pass along a reference of a supervisor for every thread you fork.


## Background

As time progresses, I've come to love developing concurrent applications in
Haskell, its API (STM, MVars, etc.) and light threading RTS bring a lot to the
table. There is another technology that is more famous than Haskell in regards
to concurrency, and that is Erlang, more specifically its OTP library.

If you wonder why that is, you may need to look into the OTP library design,
actors systems (in general) provide an architecture that enables applications to
be tolerant to failure through the enforcement of communication via message
passing and by making use of a critical infrastructure piece called a
*Supervisor*.

After trying to replicate Erlang's behavior on Haskell applications by using the
[distributed-process](https://hackage.haskell.org/package/distributed-process)
library (a clone of OTP), and after implementing several (disposable) iterations
of actor systems in Haskell, I've settled with just this library, one that
provides a simple Supervisor API.