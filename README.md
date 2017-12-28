# overseer

> Our greatest glory is not in never failing, but in rising every time we fail.– Confucius

## Table Of Contents

* [Raison d'etre](#raison-detre)
* [Documentation](#documentation)
* [Development](#development)

## Raison d'etre

As time progresses, I've come to love developing concurrent applications in
Haskell, its API (stm, mvars, etc.) and light threading RTS bring a lot to the
table. There is another technology that is more famous than Haskell in
regards to concurrency, and that is Erlang, more specifically its OTP library.

If you wonder why that is, you may need to look into the OTP library design,
actors systems (in general) provide an architecture that enables applications to
be tolerant to failure through the enforcement of communication via message
passing and by making use of a critical infrastructure piece called a *Supervisor*.

After trying to replicate Erlang's behavior on Haskell applications by using the
[distributed-process](https://hackage.haskell.org/package/distributed-process)
library (a clone of OTP), and after implementing several (disposable) iterations
of actor systems in Haskell, I've settled with just this library, one that
provides a simple Supervisor API.

This library is intended to be a drop-in replacement to `forkIO` invocations
throughout your codebase, the difference being, you'll need to do a bit more of
setup specifying supervision rules, and also pass along a reference of a
supervisor descriptor to every thread fork.

### Why not [distributed-process](https://hackage.haskell.org/package/distributed-process)?

`distributed-process` is an impressive library, and brings many great utilities
if you need to develop applications that are reliable. However, it is a
heavyweight solution that will enforce serious changes to your application. It
also optimizes its implementation around the *distributed* part of its name.
This library is intended to provide some benefits of `distributed-process` ,
without the baggage.

### Why not a complete actor system?

Actor systems are very pervasive, they impose specific design constraints on
your application which can be rather expensive. This library attempts to bring
some of the reliability benefits of actor systems without the "change all your
application to work with actors" part of the equation.

That said, this library can serve as a basis for a more prominent library that
provides an opinionated Inter-Process communication scheme. If you happen to
attempt at doing exactly that, please let me know, I would love to learn about
such initiatives.

### Why not [async](https://hackage.haskell.org/package/async)?

`async` is a fabulous library that allows Applicative composition of small
asynchronous sub-routines into bigger ones and link errors between them. Given
this, `async` fits the bill perfectly for small operations that happen
concurrently, not necessarily for long living threads. This library attempts not
to replace async's forte, but rather provides ways to make threads reliable in
situations where the usage of `async` or `forkIO` would give you the same
outcome.

## Documentation

Documentation can be found [here](https://romanandreg.gitbooks.io/haskell-supervisor/content/)

## Installation

[![Hackage](https://img.shields.io/hackage/v/supervisor.svg)](https://img.shields.io/hackage/v/supervisor.svg)
[![Stackage LTS](https://stackage.org/package/supervisor/badge/lts)](http://stackage.org/lts/package/supervisor)
[![Stackage Nightly](https://stackage.org/package/supervisor/badge/nightly)](http://stackage.org/nightly/package/supervisor)

Make sure you include the following entry on your [cabal file's
dependecies](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information)
section.

```cabal
library:
  build-depends: overseer
```

Or on your `package.yaml`

```
dependencies:
- overseer
```

## Development

[![Build Status](https://travis-ci.org/roman/Haskell-supervisor.svg?branch=master)](https://travis-ci.org/roman/Haskell-supervisor)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-supervisor/v0.0.0.1.svg)](https://img.shields.io/github/commits-since/roman/haskell-supervisor/v0.0.0.1.svg)
[![Hackage Dependencies\]](https://img.shields.io/hackage-deps/v/supervisor.svg)](https://img.shields.io/hackage/v/supervisor.svg)

### Open Commit Bit

supervisor has an open commit bit policy: Anyone with an accepted pull request
gets added as a repository collaborator. Please try to follow these simple
rules:

* Commit directly onto the master branch only for typos, improvements to the
  README and documentation.

* Create a feature branch and open a pull-request early for any new features to
  get feedback.

* Make sure you adhere to the general pull request rules above.
