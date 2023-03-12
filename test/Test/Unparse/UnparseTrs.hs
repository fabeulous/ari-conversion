-- |
-- Module      : Test.Unparse.UnparseTrs
-- Description : Unparsing tests for TRSs
--
-- This module defines test cases for converting first-order
-- TRSs from the internal 'Trs' representation to COPS and ARI format.
module Test.Unparse.UnparseTrs (unparseCopsTrsTests, unparseAriTrsTests) where

import Data.Conversion.Parser.Unparse.UnparseTrs (unparseAriTrs, unparseCopsTrs)
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Prettyprinter (Pretty)
import Test.HUnit
import Test.TestData (ariTrss, copsTrss)
import Test.Unparse.Utils (assertUnparseList)

-- | Unparser for COPS format
copsTrsUnparser :: (Eq v, Pretty f, Pretty v) => Trs f v -> String
copsTrsUnparser trs = case unparseCopsTrs trs of
  Right unparsed -> show unparsed
  Left err -> show err -- qqjf Add error handling

-- | Unparser for ARI format
ariTrsUnparser :: (Eq f, Eq v, Pretty f, Pretty v, Show f) => Trs f v -> String
ariTrsUnparser trs = case unparseAriTrs trs of
  Right unparsed -> show unparsed
  Left err -> show err -- qqjf Add error handling

-- | Tests for converting some example 'Trs's to COPS format using 'unparseCopsTrs'
unparseCopsTrsTests :: Test
unparseCopsTrsTests =
  assertUnparseList
    ([(trs, res, "unparseCopsTrs should succeed on " ++ l) | (trs, res, l) <- copsTrss])
    copsTrsUnparser

-- | Tests for converting some example 'Trs's to ARI format using 'unparseAriTrs'
unparseAriTrsTests :: Test
unparseAriTrsTests =
  assertUnparseList
    ([(trs, res, "unparseAriTrs should succeed on " ++ l) | (trs, res, l) <- ariTrss])
    ariTrsUnparser