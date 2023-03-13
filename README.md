# trs-conversion

Working repository for term rewriting system (TRS) format conversion as part of the [ARI](https://ari-informatik.uibk.ac.at/) project. This repository contains Haskell programs to convert term rewriting formats between the [COPS formats](http://project-coco.uibk.ac.at/problems/#format) and the new [ARI formats](https://ari-informatik.uibk.ac.at/tasks/A/).

## Getting Started

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

### Build

This project is set up using [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/). The app has entry point `trs-conversion/app/Main.hs`. To build and execute the program, run the following:

```
stack build
stack exec trs-conversion-exe
```

### Tests

Unit tests for parsing and unparsing are implemented using [HUnit](https://hackage.haskell.org/package/HUnit). Tests can be run by calling `stack test`.

### Documentation

The project is documented using [Haddock](https://haskell-haddock.readthedocs.io/en/latest/markup.html). To preview documentation in a browser, run `stack hoogle --server --no-haddock-deps [--rebuild]` and navigate to [localhost:8080](http://localhost:8080/).

## Tasks

This project supports two main functions: parsing and unparsing. _Parsing_ refers to converting input documents (such as a string in COPS TRS format) to a common internal representation. _Unparsing_ refers to converting from this Haskell representation format into a desired output format (for example, the new ARI TRS format).

### Dependencies

Parsing is implemented using [Megaparsec](https://hackage.haskell.org/package/megaparsec) (a well-documented monadic parser). [This tutorial](https://markkarpov.com/tutorial/megaparsec.html) provides enough information to get started with. Unparsing is implemented with [prettyprinter](https://hackage.haskell.org/package/prettyprinter) (a Wadler/Leijen pretty printer).

### Architecture

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

## Disclaimer

This is still an early version and has not been tested extensively on real-world examples. Comments marked with `qqjf` indicate decisions or assumptions that might be liable to change.
