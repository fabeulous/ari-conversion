module Test.Parse.Rule (ruleTests) where -- qqjf module description

import Data.Conversion.Parser.Parse.Problem.Rule (parseRule, parseRules)
import Data.Conversion.Parser.Parse.Utils (Parser)
import Data.Conversion.Problem.Rule (Rule (..))
import Data.Conversion.Problem.Term (Term (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

ruleTests :: Test
ruleTests = TestList [parseRuleTests, badRulesTests, parseMultipleRules]

-- | qqjf parser for testing with a given set of variables
ruleParser :: Parser (Rule String String)
ruleParser = parseRule ["x", "y", "z", "x'"]

-- | qqjf
parseRuleTests :: Test
parseRuleTests = assertParseList validRules ruleParser
  where
    fx = Fun "f" [Var "x"]
    -- \| Rules which should be parseable
    -- Non-exhaustive, but covers most common cases
    validRules :: [(String, Rule String String)]
    validRules =
      [ ("f(x)->x", Rule {lhs = fx, rhs = Var "x"}),
        (" f(x)->x", Rule {lhs = fx, rhs = Var "x"}),
        ("f(x)->x ", Rule {lhs = fx, rhs = Var "x"}),
        ("  f(x)  ->    x ", Rule {lhs = fx, rhs = Var "x"}),
        ("x->f(x)", Rule {lhs = Var "x", rhs = fx}),
        ("f(x)->f(x)", Rule {lhs = fx, rhs = fx}),
        ("x->x", Rule {lhs = Var "x", rhs = Var "x"}), -- Currently allowed
        ("a()->b", Rule {lhs = Fun "a" [], rhs = Fun "b" []})
      ]

-- | qqjf
badRulesTests :: Test
badRulesTests = assertFailParseList badRules ruleParser
  where
    -- \| Example rules which should not be parseable when parsing the enture input
    -- This list is non-exhaustive, but provides some simple sanity checks
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

-- | qqjf Parse multiple rules (used in TRS parsing)
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
