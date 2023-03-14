# trs-conversion

Repository for term rewriting system (TRS) format conversion as part of the [ARI](https://ari-informatik.uibk.ac.at/) project. This repository contains Haskell programs to convert term rewriting formats between existing [COPS formats](http://project-coco.uibk.ac.at/problems/#format) and the new [ARI formats](https://ari-informatik.uibk.ac.at/tasks/A/).

### Table of contents

- [Getting Started](#getting-started)
  - [Build](#build)
  - [Documentation](#documentation)
  - [Code Structure](#code-structure)
  - [Tests](#tests)
- [Implementation](#implementation)
  - [Dependencies](#dependencies)
  - [Examples](#examples)
    - [Running a Parser](#running-a-parser)
    - [Testing a Parser](#testing-a-parser)
    - [Unparsing a TRS](#unparsing-a-trs)
- [Architecture](#architecture)
  - [Extensibility](#extensibility)
    - [Adding a New Problem Type](#adding-a-new-problem-type)
    - [Adding a New Conversion Format](#adding-a-new-conversion-format)
  - [Limitations](#limitations)
  - [Disclaimer](#disclaimer)

<small style="font-size: 7px"><i>Table of contents generated with <a href='http://ecotrust-canada.github.io/markdown-toc/'>markdown-toc</a>.</i></small>

## Getting Started

This project is set up using [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/). Install Haskell and Stack (e.g. by installing [GHCup](https://www.haskell.org/ghcup/)), then check that Stack is installed by typing `stack --version` into the command line.

### Build

The project has entry point [`app/Main.hs`](app/Main.hs). To build the project and execute [`Main.hs`](app/Main.hs), run the following:

```
stack build
stack exec trs-conversion-exe
```

### Documentation

The project is primarily documented inline using [Haddock](https://haskell-haddock.readthedocs.io/en/latest/markup.html). To preview documentation in a browser, run `stack hoogle --server --no-haddock-deps [--rebuild]` and navigate to [localhost:8080](http://localhost:8080/). See `stack hoogle --help` for more information about arguments.

### Folder Structure

The project has the following directory structure:

```
└──trs-conversion
    ├───app                  -- The entry point of the app
    ├───src/Data/Conversion  -- Main source code
    │    ├───Parse           -- Parsing functionality
    │    ├───Problem         -- Haskell implemementation of rewriting systems
    │    └───Unparse         -- Unparsing (pretty-printing) functionality
    └───test/Test            -- Unit tests
        ├───Parse            -- Tests for parsing
        ├───TestData         -- Test data used for both parsing and unparsing
        └───Unparse          -- Tests for unparsing
```

### Tests

Unit tests for parsing and unparsing are implemented using [HUnit](https://hackage.haskell.org/package/HUnit) and have entry point [`test/Spec.hs`](test/Spec.hs). Tests can be run by calling `stack test`.

## Implementation

This library supports two main functions: parsing and unparsing. **Parsing** refers to converting input documents (such as a string in COPS TRS format) to a Haskell representation. **Unparsing** refers to converting from this Haskell representation format into a desired output format (such as the new ARI TRS format).

### Dependencies

Parsing is implemented using [Megaparsec](https://hackage.haskell.org/package/megaparsec) (a monadic parser). [This tutorial](https://markkarpov.com/tutorial/megaparsec.html) provides enough information to get started with MegaParsec. Unparsing is implemented with [prettyprinter](https://hackage.haskell.org/package/prettyprinter) (a Wadler/Leijen pretty printer).

Dependencies are managed in the [`package.yaml`](package.yaml) and [`stack.yaml`](stack.yaml) files.

### Examples

Many examples of expected input and output can be found in the [tests](test/Spec.hs).

#### Running a Parser

The following function `parsingExample` can be used to parse a `String` into a `Trs`, for example by calling `parsingExample "(VAR x y)(RULES  f(x,y) -> g(c))"`.

```

import Data.Conversion.Parse.ParseTrs (parseCopsTrs)
import Data.Conversion.Problem.Trs.Trs (Trs)
import Data.Text (pack)
import Text.Megaparsec (errorBundlePretty, parse)

parsingExample :: String -> Either String (Trs String String)
parsingExample input = case parse parseCopsTrs "COPS Example" (pack input) of
  Left err -> Left $ errorBundlePretty err
  Right trs -> return trs
```

- The type `Either` is used as parsing might fail (in which case an error should be shown)
- `pack` transforms a `String` into `Text`
- `errorBundlePretty` is used to pretty print a MegaParsec error if parsing fails.

#### Testing a Parser

The MegaParsec function [`parseTest`](https://hackage.haskell.org/package/megaparsec-9.3.0/docs/Text-Megaparsec.html#v:parseTest) can be used to quickly test parsers and print output to the terminal during development.

```
{-# LANGUAGE OverloadedStrings #-} -- Allow using Strings as Text

import Data.Conversion.Parse.Problem.Rule (parseCopsRule)
import Text.Megaparsec (parseTest)

testRule :: IO ()
testRule = parseTest (parseCopsRule ["x", "y"]) "f(x,c)->x"
```

Calling `testRule` will print `Rule {lhs = Fun "f" [Var "x",Fun "c" []], rhs = Var "x"}` to the console.

#### Unparsing a TRS

```
import Data.Conversion.Problem.Common.MetaInfo (emptyMetaInfo)
import Data.Conversion.Problem.Trs.Trs (Rule (..), Term (..), Trs (..), TrsSig (..))
import Data.Conversion.Unparse.UnparseTrs (unparseAriTrs)

exampleTrs :: Trs String String
exampleTrs =
  Trs
    { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "g" [Fun "c" []]}],
      signature = Vars ["x", "y"],
      metaInfo = emptyMetaInfo
    }

exampleAriTrs :: IO ()
exampleAriTrs = case unparseAriTrs exampleTrs of
  Left err -> print err -- Add error handling
  Right out -> print out
```

Calling `exampleAriTrs` will print the following to the console:

```
(format TRS)
(fun f 2)
(fun g 1)
(fun c 0)
(rule (f x y) (g c))
```

## Architecture

The project uses a strict layered architecture:

- The datatypes and helper functions for the Haskell representations of term rewriting systems are defined in [Data.Conversion.Problem](src/Data/Conversion/Problem)
- Parsing functions are defined in [Data.Conversion.Parse](src/Data/Conversion/Parse)
- Unparsing functions are defined in [Data.Conversion.Unparse](src/Data/Conversion/Unparse)

```
┏━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ Data.Conversion.Parse ┃ Data.Conversion.Unparse ┃↖
┣━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┫ ↑
┃             Data.Conversion.Problem             ┃↗
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

In particular,

- Files in `Data.Conversion.Problem` should never import from `Data.Conversion.Parse` or `Data.Conversion.Unparse`
- Files in `Data.Conversion.Parse` should never import from `Data.Conversion.Unparse` or vice versa

### Extensibility

The project is set up to be extensible to new classes of problem and conversion formats as needed.

#### Adding a New Problem Type

Support for a new class of rewriting system can be added as follows:

1. Create a datatype `NewTrs` for the rewriting system in [Data.Conversion.Problem](src/Data/Conversion/Problem). See existing datatypes for examples.
2. Create parsers of type `Parser NewTrs` in [Data.Conversion.Parse](src/Data/Conversion/Parse) for each supported input format.
3. Create unparsers of type `NewTrs -> Either String (Doc ann)` in [Data.Conversion.Unparse](src/Data/Conversion/Unparse) for each supported output format.
4. Add unit tests in the [test](test/Spec.hs) directory.

#### Adding a New Conversion Format

Support for a new format for an existing rewriting system `SomeTrs` can be added as follows:

1. Add a function of type `Parser SomeTrs` to the existing parsing functions in [Data.Conversion.Parse](src/Data/Conversion/Parse).
2. Add a function of type `SomeTrs -> Either String (Doc ann)` to the existing unparsing functions in [Data.Conversion.Unparse](src/Data/Conversion/Unparse).
3. Extend the existing tests for `SomeTrs` to assert that the new parsing and unparsing functions work as expected.

### Limitations

The parsing module currently only check that a given input _can_ be parsed. It does not check that the system itself makes sense. In particular, the following should possibly be checked for a well-defined rewriting system:

- that the set of function symbols and variables are disjoint
- that function symbols are always applied with a correct and/or consistent arity
- type correctness (for many-sorted TRSs)
- that variables on the right-hand side of a `Rule` also appear in the left-hand side of a rule

### Disclaimer

This is still an early version and has not been tested extensively on real-world examples. Comments marked with `qq[initals]` indicate decisions or assumptions that might be liable to change.
