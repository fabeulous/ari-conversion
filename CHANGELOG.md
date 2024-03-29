# Changelog for `trs-conversion`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased
- change LCTRS (format ..) tag syntax to include ':smtlib'. See:
  http://project-coco.uibk.ac.at/ARI/lctrs.php

## 0.6.0.0 - 2023-12-28
- fixes to COPS output on CSCTRSs
- change LCTRS (format ..) tag syntax to include ':standard'. See:
  http://project-coco.uibk.ac.at/ARI/lctrs.php

## 0.5.0.0 - 2023-12-15
- many small fixes to the ARI parser
- `file-type` executable added. Given an ARI input file it prints the format
  type to stdout.
- dropped `--comm` as shorthand for the `--commutation` flag, since the parser
  already uses flags when the prefix matches only a single option
- ARI parser no longer allows most keywords as function symbols or variables
- added INFEASIBILITY format for both COPS and ARI
- `ari-syntax-checker` executable added.
- only allow ASCII characters in identifiers
  (specifically printable characters not in " ;:()")
- allow leading spaces before meta-info comments
- allow noraml comments before and between meta-info comments
- support for pretty printing the CPF3-input format of CeTA added
- add flag for '@cops' meta info field

## 0.4.0.0 - 2023-08-14
- ARI format support for multiple systems
- new flag '--comm' for printing COMMUTATION problems in COPS
- COPS format supports COMMUTATION problems
- new '--version' flag

## 0.3.0.0 - 2023-05-26
- restructure Parser modules to separate COPS and ARI parsers
- Add CTrs and CSTrs problem types
- Change ARI `(meta-info ..)` syntax.
  meta info is now stored in a leading comment using `; @key value` pairs
- Meta data parsing from COPS `(COMMENT .. )` block.
  The tool now parses DOIs and "submitted by:" lines.
- Now support ARI format as described in the IWC23 submission

## 0.2.0.0 - 2023-03-14

### Added

- Support for parsing and unparsing TRSs and MSTRSs from and to COPS and ARI formats
- Extensive unit tests using HUnit
- Haddock documentation and README for project handover
