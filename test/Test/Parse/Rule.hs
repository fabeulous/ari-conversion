-- |
-- Module      : Test.Parse.Rule
-- Description : Parsing tests for rules
--
-- This module defines test cases for the functions 'parseRule' and 'parseRules'. Tests are non-exhaustive, but cover common cases and some useful checks.
module Test.Parse.Rule (ruleTests) where

import Data.Conversion.Parser.Parse.Problem.Rule (parseRule, parseRules)
import Data.Conversion.Parser.Parse.Utils (Parser)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Tests for parsing rules, including tests for which parsing should succeed and for which parsing should fail
ruleTests :: Test
ruleTests = TestList [parseRuleTests, badRulesTests, parseMultipleRules]

-- | A rule parser for testing with a fixed set of variables
ruleParser :: Parser (Rule String String)
ruleParser = parseRule ["x", "y", "z", "x'"]

-- | Test cases for which 'parseRule' should succeed and match the given expected output
parseRuleTests :: Test
parseRuleTests = assertParseList validRules ruleParser
  where
    fx = Fun "f" [Var "x"]
    validRules :: [(String, Rule String String)]
    validRules =
      [ ("f(x)->x", Rule {lhs = fx, rhs = Var "x"}),
        (" f(x)->x", Rule {lhs = fx, rhs = Var "x"}),
        ("f(x)->x ", Rule {lhs = fx, rhs = Var "x"}),
        ("  f(x)  ->    x ", Rule {lhs = fx, rhs = Var "x"}),
        ("x->f(x)", Rule {lhs = Var "x", rhs = fx}),
        ("f(x)->f(x)", Rule {lhs = fx, rhs = fx}),
        ("x->x", Rule {lhs = Var "x", rhs = Var "x"}), -- qqjf Currently allowed
        ("a()->b", Rule {lhs = Fun "a" [], rhs = Fun "b" []})
      ]

-- | Example test cases of malformatted rules for which parsing should fail when parsing the entire input
-- Non-exhaustive, but intended to be used as a sanity check
badRulesTests :: Test
badRulesTests = assertFailParseList badRules ruleParser
  where 
    badRules :: [String]
    badRules =
      [ "f(x,y)",
        "f(x,y) g(x)",
        "f(x)->",
        "->c",
        "f(x)->g(x)->h(x)",
        "f(x)->->g(x)",
        "f(x)g(x)->g(x)",
        "f(x)->g(x)h(x)",
        "f(x)<-g(x)",
        "(f(x)->x)",
        "",
        " ",
        "->",
        "\n"
      ]

-- | Tests for the 'parseRules' function (used in TRS parsing to parse blocks containing 0 or more rules).
-- Asserts that the test cases are parseable and match the expected output.
parseMultipleRules :: Test
parseMultipleRules = assertParseList validRules rulesParser
  where
    rulesParser :: Parser [Rule String String]
    rulesParser = do
      (rs, _) <- parseRules ["x", "y", "z", "x'"] Nothing
      return rs
    r1 = Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}
    r2 = Rule {lhs = Fun "g" [Var "x", Var "y"], rhs = Fun "f" [Var "y"]}
    r3 = Rule {lhs = Fun "a" [], rhs = Fun "b" []}
    -- \| Example strings containing multiple rules which should be parseable
    validRules :: [(String, [Rule String String])]
    validRules =
      [ ("f(x)->x g(x,y)->f(y)    a->b", [r1, r2, r3]),
        ("f(x)->x g(x,y)->f(y)\na->b", [r1, r2, r3]),
        ("a->b\na->b", [r3, r3]),
        ("", [])
      ]
