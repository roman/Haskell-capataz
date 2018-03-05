# Capataz

> Our greatest glory is not in never failing, but in rising every time we fail.– Confucius

## Table Of Contents

* [Installation](#installation)
* [Documentation](#documentation)
* [Development](#development)

## Installation

[![Hackage](https://img.shields.io/hackage/v/capataz.svg)](https://img.shields.io/hackage/v/capataz.svg)
[![Stackage LTS](https://www.stackage.org/package/capataz/badge/lts)](http://stackage.org/lts/package/capataz)
[![Stackage Nightly](https://www.stackage.org/package/capataz/badge/nightly)](http://stackage.org/nightly/package/capataz)

Make sure you include the following entry on your [cabal file's
dependecies](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information)
section.

```cabal
library:
  build-depends: capataz
```

Or on your `package.yaml`

```
dependencies:
- capataz
```

## Documentation

The documentation website can be found
[here](https://romanandreg.gitbooks.io/capataz/content/). Some relevant links:

* [Purpose](https://romanandreg.gitbooks.io/capataz/content/purpose.html)
* [FAQ](https://romanandreg.gitbooks.io/capataz/content/FAQ.html)

## Development

[![Build Status](https://travis-ci.org/roman/Haskell-capataz.svg?branch=master)](https://travis-ci.org/roman/Haskell-capataz)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-capataz/v0.1.0.1.svg)](https://img.shields.io/github/commits-since/roman/haskell-capataz/v0.1.0.1.svg)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/capataz.svg)](http://packdeps.haskellers.com/feed?needle=capataz)

Follow the [developer guidelines](https://romanandreg.gitbooks.io/capataz/content/CONTRIBUTING.html)

## In future releases

* Replace Protolude in favor of RIO
* Documentation of performance analysis
* Ensure unit tests always finish on all concurrent scenarios (dejafu experiment)
