-- |
-- Module      : Test.Parse.Term
-- Description : Parsing tests for terms
--
-- This module defines test cases for the function 'parseTerm'. Tests are non-exhaustive, but cover common cases and some useful checks.
module Test.Parse.Term (termTests) where

import Data.Conversion.Parser.Parse.Problem.Term (parseTerm)
import Data.Conversion.Parser.Parse.Utils (Parser)
import Data.Conversion.Problem.Term (Term (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Tests for 'parseTerm' including tests for which parsing should succeed and for which parsing should fail
termTests :: Test
termTests = TestList [parseTermTests, parenthesesTests, malformattedTermTests]

-- | Parser for testing 'parseTerm' with a fixed set of variables
termParser :: Parser (Term String String)
termParser = parseTerm ["x", "y", "z", "x'"]

-- | Tests for cases when 'parseTerm' should succeed and produce a term as output
parseTermTests :: Test
parseTermTests = assertParseList wellFormattedTerms termParser
  where
    -- Terms which should be parseable (non-exhaustive) and their expected Haskell representation
    wellFormattedTerms :: [(String, Term String String)]
    wellFormattedTerms =
      [ ("x", Var "x"),
        ("c", Fun "c" []),
        ("f(x)", Fun "f" [Var "x"]),
        ("f(f(f(x)))", Fun "f" [Fun "f" [Fun "f" [Var "x"]]]),
        ("f(x')", Fun "f" [Var "x'"]),
        ("f(x,y, z)", Fun "f" [Var "x", Var "y", Var "z"]),
        ("f(c,y,z) ", Fun "f" [Fun "c" [], Var "y", Var "z"]),
        (" f(c,f(g))", Fun "f" [Fun "c" [], Fun "f" [Fun "g" []]]),
        ("f(g(d,e),y)", Fun "f" [Fun "g" [Fun "d" [], Fun "e" []], Var "y"]),
        ("f(x,g(d,e))", Fun "f" [Var "x", Fun "g" [Fun "d" [], Fun "e" []]]),
        ("f(x,b(d,e),y)", Fun "f" [Var "x", Fun "b" [Fun "d" [], Fun "e" []], Var "y"]),
        ("f()", Fun "f" []),
        ("+(x,y)", Fun "+" [Var "x", Var "y"]),
        ("(x)", Var "x"),
        ("(c)", Fun "c" []),
        ("f(xy)", Fun "f" [Fun "xy" []])
        -- ("-(x,y)", Fun "-" [Var "x", Var "y"]),
        -- ("((c))", Fun "c" []),
        -- ("(((x)))", Var "x"),
      ]

-- | Tests in which the term parentheses are malformatted.
-- When parsing /the entire string/, then parsing should fail for these examples.
parenthesesTests :: Test
parenthesesTests = assertFailParseList badParentheses termParser
  where
    badParentheses :: [String]
    badParentheses =
      [ "((c)",
        "(c))",
        "f((c,y,z)",
        "f(c,y,z))",
        "f(x,) g(y))",
        "f(c,(y,z)",
        "f(c,)y,z)"
      ]

-- | Tests in which the given example is not a valid term.
-- It is asserted that parsing should fail for these examples.
malformattedTermTests :: Test
malformattedTermTests = assertFailParseList badTerms termParser
  where
    badTerms :: [String]
    badTerms =
      [ ",c",
        "c,",
        "c,y",
        "(y,z)",
        "f x",
        "f(x,,y)",
        "f(",
        "f)",
        "f(x(",
        "f(x)->x",
        "",
        " ",
        "\n"
      ]
