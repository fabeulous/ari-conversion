# Changelog for `trs-conversion`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased
- ARI format supports for multiple systems
  (not the case yet for COPS)

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
