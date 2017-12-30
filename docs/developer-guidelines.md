# Developer Guidelines
[![Build Status](https://travis-ci.org/roman/Haskell-capataz.svg?branch=master)](https://travis-ci.org/roman/Haskell-capataz)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-capataz/v0.0.0.0.svg)](https://img.shields.io/github/commits-since/roman/haskell-capataz/v0.0.0.0.svg)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/capataz.svg)](https://img.shields.io/hackage/v/capataz.svg)

## Dependencies

You'll need to install [Stack](https://github.com/commercialhaskell/stack), once installed, you can execute the `make` command.

You'll need to make sure you invoke `make format` and `make lint` when pushing changes, otherwise the Pull Request builder will fail.

## General Overview

This project heavily relies in two (2) Haskell extensions, [`NamedFieldPuns`]() and [`DuplicateRecordFields`]().

You can tell that _many_ records share the same fields, this is because these fields represent the same data in different contexts. This makes IMO the code more readable because we don't use different names (say, with a prefixes) to represent the same piece of information.

However, this has the unfortunate side-effect that when using the field name as a function, we get ambiguous symbol errors from the compiler. To alliviate this, we only access the fields through _field pun_ notation.

The code has been throughly documented, if you believe the documentation could be better, please create a ticket in Github with suggestions.

## Notes on testsuite

All tests related to this API are in a single module `Control.Concurrent.CapatazTest`, in this file we have defined:

* Assertion functions to get attributes from a `CapatazEvent`

* Helpers to run the test (reduce boilerplate)

* Actual tests

The module contains documentation for all the helper functions, and hopefully the test descriptions should be
enough to understand what is being tested.

If you have strong opinions about this testing approach, please reach out, I'm trying to validate if this is a good idea or not.

## Open Commit Policy

This library has an open commit bit policy: Anyone with an accepted pull request gets added as a repository collaborator.

Please create a feature branch and open a pull-request early for any new features or documentation improvements.
