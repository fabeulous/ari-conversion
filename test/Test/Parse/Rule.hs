-- |
-- Module      : Test.Parse.Rule
-- Description : Parsing tests for rules
--
-- This module defines test cases for functions used to parse TRS rules.
-- Tests are non-exhaustive, but cover common cases and some useful checks.
module Test.Parse.Rule (parseRuleTests) where

import Data.Conversion.Parse.Problem.Rule (parseAriRule, parseCopsMsTrsRules, parseCopsRule, parseCopsTrsRules)
import Data.Conversion.Parse.Utils (Parser)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.MsTrs.MsSig (MsSig (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Tests for parsing rules in COPS and ARI formats
parseRuleTests :: Test
parseRuleTests = TestLabel "Test.Parse.Rule" $ TestList [copsRuleTests, ariRuleTests]

-- | Tests for parsing rules in COPS format, including tests for which parsing should succeed and for which parsing should fail
copsRuleTests :: Test
copsRuleTests = TestLabel "copsRuleTests" $ TestList [parseCopsRuleTests, parseCopsRulesFTests, badCopsRulesTests, parseMultipleCopsRules]

-- | Tests for parsing rules in ARI format, including tests for which parsing should succeed and for which parsing should fail
ariRuleTests :: Test
ariRuleTests = TestLabel "ariRuleTests" $ TestList [parseAriRuleTests, badAriRulesTests]

-- | A rule parser for testing 'parseCopsRule' with a fixed set of variables
copsRuleParser :: Parser (Rule String String)
copsRuleParser = parseCopsRule ["x", "y", "z", "x'"]

-- | A rule parser for testing 'parseCopsMsTrsRules' with a fixed set of function symbols
copsRuleFParser :: Parser [Rule String String]
copsRuleFParser = parseCopsMsTrsRules [MsSig "f" (["Nat"], "Nat"), MsSig "fun" (["Nat"], "Nat"), MsSig "a" ([], "Type"), MsSig "b" ([], "Type")]

-- | Test cases for which 'parseCopsRule' should succeed and
-- match the given expected output. Tests parsing COPS rules when the variables are known.
parseCopsRuleTests :: Test
parseCopsRuleTests = assertParseList "parseCopsRule should succeed" validRules copsRuleParser
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
        ("x->x", Rule {lhs = Var "x", rhs = Var "x"}),
        ("a()->b", Rule {lhs = Fun "a" [], rhs = Fun "b" []})
      ]

-- | Test cases for which 'parseCopsMsTrsRules' should succeed and
-- match the given expected output.  Tests parsing COPS rules when the function symbols are known.
parseCopsRulesFTests :: Test
parseCopsRulesFTests = assertParseList "parseCopsMsTrsRules should succeed" validRules copsRuleFParser
  where
    fx = Fun "f" [Var "x"]
    validRules :: [(String, [Rule String String])]
    validRules =
      [ ("f(x)->x", [Rule {lhs = fx, rhs = Var "x"}]),
        ("  f(x)  ->    x ", [Rule {lhs = fx, rhs = Var "x"}]),
        ("x->f(x)", [Rule {lhs = Var "x", rhs = fx}]),
        ("fun(fx)->fun(fx)", [Rule {lhs = Fun "fun" [Var "fx"], rhs = Fun "fun" [Var "fx"]}]),
        ("x->x", [Rule {lhs = Var "x", rhs = Var "x"}]),
        ("a()->b", [Rule {lhs = Fun "a" [], rhs = Fun "b" []}])
      ]

-- | Example test cases of malformatted rules for which parsing should fail
-- when parsing the entire input according to COPS format.
-- Non-exhaustive, but intended to be used to check that certain clearly wrong formats are not accepted.
badCopsRulesTests :: Test
badCopsRulesTests = assertFailParseList "parseCopsRule should fail" badRules copsRuleParser
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
        "(f x) (x)", -- ARI format
        "",
        " ",
        "->",
        "\n",
        "somestring"
      ]

-- | Tests for the 'parseCopsTrsRules' function (used in COPS TRS parsing
-- to parse blocks containing 0 or more rules).
-- Asserts that the test cases are parseable and match the expected output.
parseMultipleCopsRules :: Test
parseMultipleCopsRules = assertParseList "parseCopsTrsRules should succeed" validRules rulesParser
  where
    rulesParser :: Parser [Rule String String]
    rulesParser = parseCopsTrsRules $ Vars ["x", "y", "z", "x'"]
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

-- | A rule parser for testing 'parseAriRule' with a fixed function signature
ariRuleParser :: Parser (Rule String String)
ariRuleParser =
  parseAriRule
    [ Sig "0" 0,
      Sig "s" 1,
      Sig "nil" 0,
      Sig ":" 2,
      Sig "f" 2
    ]

-- | Test cases for which 'parseAriRule' should succeed and
-- match the given expected output
parseAriRuleTests :: Test
parseAriRuleTests = assertParseList "parseAriRule should succeed" validRules ariRuleParser
  where
    sx = Fun "s" [Var "x"]
    validRules :: [(String, Rule String String)]
    validRules =
      [ ("(s x) (x)", Rule {lhs = sx, rhs = Var "x"}),
        ("  ( s   x  )   ( x )", Rule {lhs = sx, rhs = Var "x"}),
        ("  (s sx) (x)", Rule {lhs = Fun "s" [Var "sx"], rhs = Var "x"}),
        ("x (s   x)", Rule {lhs = Var "x", rhs = sx}),
        ("(s x)  ( s x )", Rule {lhs = sx, rhs = sx}),
        ("(x) (x)", Rule {lhs = Var "x", rhs = Var "x"}),
        ("(0) (nil)", Rule {lhs = Fun "0" [], rhs = Fun "nil" []}),
        ("(f x y) s x", Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = sx}),
        ("0 (s x)", Rule {lhs = Fun "0" [], rhs = sx}),
        ("(s x) x", Rule {lhs = sx, rhs = Var "x"})
      ]

-- | Example test cases of malformatted rules for which parsing should fail
-- when parsing the entire input according to ARI format with 'parseAriRule.
-- Non-exhaustive, but intended to be used as a sanity check.
badAriRulesTests :: Test
badAriRulesTests = assertFailParseList "parseAriRule should fail" badRules ariRuleParser
  where
    badRules :: [String]
    badRules =
      [ "f x y",
        "s(x)->s",
        "(f x y)",
        "f x y s x",
        "f x y (s x)",
        "(s x)",
        "(0)",
        " (0) ",
        "(f x y) (: x y) (s x)",
        "() (s x)",
        "(s x) ()",
        "() ()",
        "(s x)(x)",
        "((f x y) (s x))",
        "",
        " ",
        "\n"
      ]
