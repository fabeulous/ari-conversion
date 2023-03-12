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
unparseRuleTests = TestLabel "unparseRuleTests" $ TestList [unparseCopsRuleTests, unparseAriRuleTests]

-- | Test unparsing example 'Rule's to COPS format using 'unparseCopsRule'
unparseCopsRuleTests :: Test
unparseCopsRuleTests = assertUnparseList testRules (show . unparseCopsRule)
  where
    testRules :: [(Rule String String, String, String)]
    testRules = [(r, expectedCops, label ++ " [COPS]") | (label, r, expectedCops, _) <- exampleRules]

-- | Test unparsing example 'Rule's to ARI format using 'unparseAriRule'
unparseAriRuleTests :: Test
unparseAriRuleTests = assertUnparseList testRules (show . unparseAriRule)
  where
    testRules :: [(Rule String String, String, String)]
    testRules = [(r, expectedAri, label ++ " [ARI]") | (label, r, _, expectedAri) <- exampleRules]

------------------------
--- Test data ----------
------------------------

-- | Example 'Rule's for testing.
-- Consists of tuples @(test label, rule, COPS format, ARI format)@
exampleRules :: [(String, Rule String String, String, String)]
exampleRules =
  [ ("Unparse rule with a variable lhs", Rule {lhs = Var "x", rhs = Var "x"}, "x -> x", "x x"),
    ("Unparse unary function application", Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}, "f(x) -> x", "(f x) x"),
    ("Unparse function application on rhs", Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "g" [Var "x"]}, "f(x,y) -> g(x)", "(f x y) (g x)"),
    ("Unparse constant-to-constant rule", Rule {lhs = Fun "a" [], rhs = Fun "b" []}, "a -> b", "a b"),
    ("Unparse constant to non-constant rule", Rule {lhs = Fun "a" [], rhs = Fun "f" [Fun "0" []]}, "a -> f(0)", "a (f 0)"),
    ( "Unparse nested unary functions",
      Rule {lhs = Fun "inc" [Fun "tl" [Fun "nats" []]], rhs = Fun "tl" [Fun "inc" [Fun "nats" []]]},
      "inc(tl(nats)) -> tl(inc(nats))",
      "(inc (tl nats)) (tl (inc nats))"
    ),
    ( "Unparse ternary function",
      Rule {lhs = Fun "f" [Var "x", Fun "g" [Var "y"], Var "z"], rhs = Fun "f" [Var "x", Var "z", Fun "g" [Var "y"]]},
      "f(x,g(y),z) -> f(x,z,g(y))",
      "(f x (g y) z) (f x z (g y))"
    )
  ]