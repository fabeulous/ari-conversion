-- |
-- Module      : Test.Unparse.UnparseTrs
-- Description : Unparsing tests for TRSs
--
-- This module defines test cases for converting first-order
-- TRSs from the internal 'Trs' representation to COPS and ARI format.
module Test.Unparse.UnparseTrs (unparseCopsTrsTests) where

import Data.Conversion.Parser.Unparse.UnparseTrs (unparseAri, unparseCops)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Test.HUnit

trs1 :: Trs String String
trs1 =
  Trs
    { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "g" [Fun "c" []]}],
      signature = Vars ["x", "y"],
      metaInfo = emptyMetaInfo
    }

trs2 :: Trs String String
trs2 =
  Trs
    { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "y"}],
      signature = FullSig ["x", "y"] [Sig "f" 2, Sig "a" 0, Sig "b" 1],
      metaInfo = emptyMetaInfo {comments = Just ["A TRS (with SIG given)"]}
    }

ssxs =
  [ " (VAR x y) \
    \ (SIG (f 2) (a 0) (b 1)) \
    \ (RULES f(x,y)->y) \
    \ (COMMENT A TRS (with SIG given)) "
  ]

-- | Unparser for COPS format
unparseCopsTrs :: (Show f, Show v) => Trs f v -> String
unparseCopsTrs trs = show $ unparseCops trs

-- | qqjf unparser tester
assertUnparse :: Show a => a -> (a -> String) -> String -> Assertion
assertUnparse obj unparser expected = assertEqual (show obj ++ " not parsed correctly") expected (unparser obj)

-- | qqjf
assertUnparseList :: Show a => [(a, String)] -> (a -> String) -> Test
assertUnparseList xs p = TestList [TestCase (assertUnparse val p expected) | (val, expected) <- xs]

-- | Tests for converting a 'Trs' to COPS format
unparseCopsTrsTests :: Test
unparseCopsTrsTests = assertUnparseList ts unparseCopsTrs
  where
    ts :: [(Trs String String, String)]
    ts = [(trs1, "(VAR x y)\n(RULES f(x,y)->g(c))")]
