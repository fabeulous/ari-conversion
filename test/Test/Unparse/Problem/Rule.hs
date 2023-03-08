-- |
-- Module      : Test.Unparse.Problem.Rule
-- Description : Unparsing tests for rules
--
-- This module defines test cases for unparsing 'Rules's into
-- COPS and ARI formats.
module Test.Unparse.Problem.Rule (unparseRuleTests) where

import Data.Conversion.Parser.Unparse.Problem.Rule (unparseAriRule, unparseCopsRule)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Test.HUnit
import Test.Unparse.Utils (assertUnparseList)

unparseRuleTests :: Test
unparseRuleTests = TestList [unparseCopsRuleTests, unparseAriRuleTests]

-- | qqjf
unparseCopsRuleTests :: Test
unparseCopsRuleTests = assertUnparseList testRules (show . unparseCopsRule)
  where
    testRules :: [(Rule String String, String)]
    testRules = [(t, expected) | (t, expected, _) <- exampleRules]

-- | qqjf
unparseAriRuleTests :: Test
unparseAriRuleTests = assertUnparseList testRules (show . unparseAriRule)
  where
    testRules :: [(Rule String String, String)]
    testRules = [(t, expected) | (t, _, expected) <- exampleRules]

------------------------
--- Test data ----------
------------------------

-- | Example 'Rule's for testing.
-- Consists of tuples @(rule, COPS format, ARI format)@
exampleRules :: [(Rule String String, String, String)]
exampleRules =
  [ (Rule {lhs = Var "x", rhs = Var "x"}, "x -> x", "x x"),
    (Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}, "f(x) -> x", "(f x) x"),
    (Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "g" [Var "x"]}, "f(x,y) -> g(x)", "(f x y) (g x)"),
    (Rule {lhs = Fun "a" [], rhs = Fun "b" []}, "a -> b", "a b"),
    (Rule {lhs = Fun "a" [], rhs = Fun "f" [Fun "0" []]}, "a -> f(0)", "a (f 0)"),
    ( Rule {lhs = Fun "inc" [Fun "tl" [Fun "nats" []]], rhs = Fun "tl" [Fun "inc" [Fun "nats" []]]},
      "inc(tl(nats)) -> tl(inc(nats))",
      "(inc (tl nats)) (tl (inc nats))"
    ),
    ( Rule {lhs = Fun "f" [Var "x", Fun "g" [Var "y"], Var "z"], rhs = Fun "f" [Var "x", Var "z", Fun "g" [Var "y"]]},
      "f(x,g(y),z) -> f(x,z,g(y))",
      "(f x (g y) z) (f x z (g y))"
    )
  ]