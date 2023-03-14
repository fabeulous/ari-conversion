# trs-conversion

Working repository for term rewriting system (TRS) format conversion as part of the [ARI](https://ari-informatik.uibk.ac.at/) project. This repository contains Haskell programs to convert term rewriting formats between existing [COPS formats](http://project-coco.uibk.ac.at/problems/#format) and the new [ARI formats](https://ari-informatik.uibk.ac.at/tasks/A/).

- [Getting Started](#getting-started)
  - [Build](#build)
  - [Documentation](#documentation)
  - [Code Structure](#code-structure)
  - [Tests](#tests)
- [Tasks](#tasks)
  - [Dependencies](#dependencies)
  - [Examples](#examples)
- [Architecture](#architecture)
  - [Extensibility](#extensibility)
    - [Adding a New Format](#adding-a-new-format)
    - [Adding a New Problem Type](#adding-a-new-problem-type)
- [Limitations](#limitations)
  - [Disclaimer](#disclaimer)

## Getting Started

Install Haskell and the Haskell Tool Stack e.g. by installing [GHCup](https://www.haskell.org/ghcup/). Check that they are installed by

# Paste Your Document In Here

## And a table of contents

will be generated

## On the right

side of this page.

typing `stack --version` into the command line.

### Build

This project is set up using [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/) and has entry point [`app/Main.hs`](app/Main.hs). To build the project and execute [`Main.hs`](app/Main.hs), run the following:

```
stack build
stack exec trs-conversion-exe
```

### Documentation

The project is documented using [Haddock](https://haskell-haddock.readthedocs.io/en/latest/markup.html). To preview documentation in a browser, run `stack hoogle --server --no-haddock-deps [--rebuild]` and navigate to [localhost:8080](http://localhost:8080/). Run `stack hoogle --help` for more information about arguments.

### Code Structure

The project has the following file structure:

```
└──trs-conversion
    ├───app                  -- The entry point of the app
    ├───src/Data/Conversion  -- Main source code
    │    ├───Parse           -- Parsing functionality
    │    ├───Problem         -- Internal implemementation of rewriting systems
    │    └───Unparse         -- Unparsing (pretty-printing) functionality
    └───test/Test            -- Unit tests
        ├───Parse            -- Tests for parsing
        ├───TestData         -- Test data used for both parsing and unparsing
        └───Unparse          -- Tests for unparsing
```

### Tests

Unit tests for parsing and unparsing are implemented using [HUnit](https://hackage.haskell.org/package/HUnit) and have entry point [`test/Spec.hs`](test/Spec.hs). Tests can be run by calling `stack test`.

---

## Tasks

This code supports two main functions: parsing and unparsing. _Parsing_ refers to converting input documents (such as a string in COPS TRS format) to a common internal representation. _Unparsing_ refers to converting from this Haskell representation format into a desired output format (for example, the new ARI TRS format).

### Dependencies

Parsing is implemented using [Megaparsec](https://hackage.haskell.org/package/megaparsec) (a monadic parser). [This tutorial](https://markkarpov.com/tutorial/megaparsec.html) provides enough information to get started with MegaParsec. Unparsing is implemented with [prettyprinter](https://hackage.haskell.org/package/prettyprinter) (a Wadler/Leijen pretty printer).

### Examples

---

## Architecture

The datatypes for the internal term rewriting formats are defined in [Data.Conversion.Problem](src/Data/Conversion/Problem). These are exported and then imported in [Data.Conversion.Parse](src/Data/Conversion/Parse) and [Data.Conversion.Problem](src/Data/Conversion/Problem). The project uses a strict layered architecture:

```
┏━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ Data.Conversion.Parse  ┃  Data.Conversion.Unparse ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃              Data.Conversion.Problem              ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

- Files in `Data.Conversion.Problem` should never import functions from `Data.Conversion.Parse` or `Data.Conversion.Unparse`
- Files in `Data.Conversion.Parse` should never import functions from `Data.Conversion.Unparse` or vice versa

### Extensibility

#### Adding a New Format

#### Adding a New Problem Type

---

## Limitations

### Disclaimer

This is still an early version and has not been tested extensively on real-world examples. Comments marked with `qqjf` indicate decisions or assumptions that might be liable to change.
