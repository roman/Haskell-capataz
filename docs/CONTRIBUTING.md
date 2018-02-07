# Developer Guidelines
[![Build Status](https://travis-ci.org/roman/Haskell-capataz.svg?branch=master)](https://travis-ci.org/roman/Haskell-capataz)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-capataz/v0.1.0.1.svg)](https://img.shields.io/github/commits-since/roman/haskell-capataz/v0.1.0.1.svg)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/capataz.svg)](https://img.shields.io/hackage/v/capataz.svg)

## Dependencies

You'll need to install [Stack](https://github.com/commercialhaskell/stack), once installed, you can execute the `make` command and learn tasks supported in the project.

You'll need to make sure you invoke `make format` and `make lint` when pushing changes, otherwise the Pull Request builder will fail.

## General Overview

This project heavily relies in two (2) Haskell extensions, [`NamedFieldPuns`](https://downloads.haskell.org/~ghc/8.2.1/docs/html/users_guide/glasgow_exts.html#record-puns) and [`DuplicateRecordFields`](https://downloads.haskell.org/~ghc/8.2.1/docs/html/users_guide/glasgow_exts.html#duplicate-record-fields).

You can tell that many records share the same fields, this is because these fields represent the same data in different contexts. This makes IMO the code more readable because we don't use different names (say, with a prefixes) to represent the same piece of information.

However, this has the unfortunate side-effect that when using the field name as a function, we get ambiguous symbol errors from the compiler. To alliviate this, we only access the fields through _field pun_ notation.

Also, to avoid the requirement to use the `DuplicateRecordFields` extension on clients of the API, we provide lenses of the public API fields.

The code has been throughly documented, if you believe the documentation could be better, please create a ticket in Github with suggestions.

## Notes on testsuite

The library is tested through integration tests that collect `CapatazEvent` from the system and assert they happen, this approach works great to avoid testing internal parts of the code that can change, however, the test-suite is not stable between executions because of timing.

If you have strong opinions about this testing approach, please reach out, I'm trying to validate if this is a good idea or not.

## Open Commit Policy

This library has an open commit bit policy: Anyone with an accepted pull request gets added as a repository collaborator.

Please create a feature branch and open a pull-request early for any new features or documentation improvements.
