module Test.Parse.Term (termTests) where -- qqjf module description

import Data.Conversion.Parser.Parse.Problem.Term (parseTerm)
import Data.Conversion.Parser.Parse.Utils (Parser)
import Data.Conversion.Problem.Term (Term (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

termTests :: Test
termTests = TestList [parseTests, parenthesesTests, malformattedTermTests]

-- | qqjf parser for testing with a given set of variables
termParser :: Parser (Term String String)
termParser = parseTerm ["x", "y", "z", "x'"]

-- | qqjf
parseTests :: Test
parseTests = assertParseList wellFormattedTerms termParser
  where
    -- \| Terms which should be parseable
    -- Non-exhaustive, but covers most common cases
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

-- | qqjf
parenthesesTests :: Test
parenthesesTests = assertFailParseList badParentheses termParser
  where
    -- \| Terms with imbalanced parentheses for which parsing should fail if parsing the entire input
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

-- | qqjf
malformattedTermTests :: Test
malformattedTermTests = assertFailParseList badTerms termParser
  where
    -- \| Example terms which should not be parseable when parsing the enture input
    -- This list is non-exhaustive, but provides some simple sanity checks
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
