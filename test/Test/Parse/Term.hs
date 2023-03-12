-- |
-- Module      : Test.Parse.Term
-- Description : Parsing tests for terms
--
-- This module defines test cases for the functions 'parseTerm' and 'parsePrefixTerm'. Tests are non-exhaustive, but cover common cases and some useful checks.
module Test.Parse.Term (termTests, prefixTermTests) where

import Data.Conversion.Parser.Parse.Problem.Term (parsePrefixTerm, parseTerm, parseTermF)
import Data.Conversion.Parser.Parse.Utils (Parser)
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Tests for 'parseTerm' including tests for which parsing should succeed and for which parsing should fail
termTests :: Test
termTests = TestLabel "termTests" $ TestList [parseTermTests, badCopsTermTests]

-- | Tests for 'parsePrefixTerm' including tests for which parsing should succeed and for which parsing should fail
prefixTermTests :: Test
prefixTermTests = TestLabel "prefixTermTests" $ TestList [parsePrefixTermTests, malformattedPrefixTermTests]

-- | Parser for testing 'parseTerm' with a fixed set of variables
termParser :: Parser (Term String String)
termParser = parseTerm ["x", "y", "z", "x'"]

-- | Parser for testing 'parseTermF' with a fixed set of function symbols
termFParser :: Parser (Term String String)
termFParser = parseTermF ["a", "b", "c", "d", "e", "f", "g", "h", "+", "xy"]

-- | Parser for testing 'prefixTermParser' with a fixed set of function symbols
prefixTermParser :: Parser (Term String String)
prefixTermParser = parsePrefixTerm [Sig "a" 0, Sig "f" 1, Sig "g" 2, Sig "h" 3]

-- | Tests for cases when 'parseTerm' should succeed and produce a term as output
parseTermTests :: Test
parseTermTests =
  TestList
    [ assertParseList l wellFormattedTerms p
      | (l, p) <-
          [ ("Parsing a term with known variables", termParser),
            ("Parsing a term with known function symbols", termFParser)
          ]
    ]
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
        -- ("((c))", Fun "c" [])
        -- ("(((x)))", Var "x"),
      ]

-- | Tests in which the given example is not a valid term in COPS format.
-- It is asserted that parsing with 'termParser' and 'termFParser' should fail for these examples.
badCopsTermTests :: Test
badCopsTermTests =
  TestList
    [ assertFailParseList l badTerms p
      | (l, p) <-
          [ ("Should fail to parse term (with known variables)", termParser),
            ("Should fail to parse term (with known function symbols)", termFParser)
          ]
    ]
  where
    badTerms :: [String]
    badTerms =
      [ "((c)",
        "(c))",
        "f((c,y,z)",
        "f(c,y,z))",
        "f(x,) g(y))",
        "f(c,(y,z)",
        "f(c,)y,z)",
        ",c",
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
        "\n",
        "x(x)" -- x is in vars and function symbols
      ]

-- | Tests for which 'prefixTermParser' should succeed and match the expected output
parsePrefixTermTests :: Test
parsePrefixTermTests = assertParseList "prefixTermParser should succeed" wellFormattedTerms prefixTermParser
  where
    -- Terms which should be parseable (non-exhaustive) and their expected Haskell representation
    wellFormattedTerms :: [(String, Term String String)]
    wellFormattedTerms =
      [ ("x", Var "x"),
        ("a", Fun "a" []),
        ("f x", Fun "f" [Var "x"]),
        ("(f x)", Fun "f" [Var "x"]),
        ("f (x)", Fun "f" [Var "x"]),
        ("f (f (f x))", Fun "f" [Fun "f" [Fun "f" [Var "x"]]]),
        ("f x'", Fun "f" [Var "x'"]),
        (" g x y", Fun "g" [Var "x", Var "y"]),
        ("  g  z   a   ", Fun "g" [Var "z", Fun "a" []]),
        ("g a z ", Fun "g" [Fun "a" [], Var "z"]),
        (" f a (f z)", Fun "f" [Fun "a" [], Fun "f" [Var "z"]]),
        ("g (g x a) y", Fun "g" [Fun "g" [Var "x", Fun "a" []], Var "y"]),
        ("g x (f y)", Fun "g" [Var "x", Fun "f" [Var "y"]]),
        ("h x (g x' z ) y", Fun "h" [Var "x", Fun "g" [Var "x'", Var "z"], Var "y"]),
        ("(x)", Var "x"),
        ("(a)", Fun "a" []),
        (" f ( xy  ) ", Fun "f" [Var "xy"]),
        ("((a))", Fun "a" []),
        ("(((x)))", Var "x"),
        ("f (g x y)", Fun "f" [Fun "g" [Var "x", Var "y"]]),
        ("g (f x) (f y)", Fun "g" [Fun "f" [Var "x"], Fun "f" [Var "y"]])
      ]

-- | Tests for which 'prefixTermParser' should fail
malformattedPrefixTermTests :: Test
malformattedPrefixTermTests = assertFailParseList "prefixTermParser should fail" badTerms prefixTermParser
  where
    badTerms :: [String]
    badTerms =
      [ "f(x)",
        "g(x,y)",
        "f(",
        "f)",
        "f (x(",
        "",
        " ",
        "\n",
        "x x",
        "a " -- Trailing space is not consumed
      ]
