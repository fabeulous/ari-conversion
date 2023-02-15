# trs-conversion

Working repository for TRS format conversion as part of the [ARI](https://ari-informatik.uibk.ac.at/) project. This repository contains Haskell implementations to convert term rewriting formats between the [COPS formats](http://project-coco.uibk.ac.at/problems/#format) and the new [ARI formats](https://ari-informatik.uibk.ac.at/tasks/A/).

Set up using [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/).

## Setup

## Commands

## Build and Run

```
stack build
stack exec trs-conversion-exe
```

### Testing

Run `stack test`

### Create Documentation

Run `stack hoogle --server --no-haddock-deps [--rebuild]` and navigate to [localhost:8080](http://localhost:8080/)

### hie generation

Uses package [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) to generate the `hie.yaml` file. Needed to enable module discovery in VS Code on Windows.

```
stack install implicit-hie # Run in root directory
gen-hie > hie.yaml
```
