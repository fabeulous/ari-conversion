{-# LANGUAGE OverloadedStrings #-}

module Test.Problem.Term (termTests) where

import Data.Conversion.Problem.Term (Term (..), parseTerm)
import Data.Either (isLeft)
import Data.Text (Text, pack)
import Data.Void (Void)
import Test.HUnit
import Text.Megaparsec (eof, errorBundlePretty, parse)
import Text.Megaparsec.Error (ParseErrorBundle)

termTests :: Test
termTests = TestList [parseTests, parenthesesTests, malformattedTermTests]

-- | Parse a term from a string input where @vs@ is a list of variables
fromString :: [String] -> String -> Either (ParseErrorBundle Text Void) (Term String String)
fromString vs t = parse (parseTerm vs <* eof) "" (pack t)

-- | Assert that parsing string @t@ as a term with variable set @vs@ fails
assertParseFail :: [String] -> String -> Assertion
assertParseFail vs t = assertBool ("Term " ++ t ++ " should not be parsed") (isLeft $ fromString vs t)

-- | Assert that parsing string @t@ as a term with variable set @vs@ returns term @res@
assertParse :: [String] -> String -> Term String String -> Assertion
assertParse vs t expected = case fromString vs t of
  Left err -> assertFailure (errorBundlePretty err)
  Right result -> assertEqual ("Term " ++ t ++ " not parsed correctly") expected result

vars :: [String]
vars = ["x", "y", "z", "x'"]

parseTests :: Test
parseTests = TestList [TestCase (assertParse vars t expected) | (t, expected) <- wellFormattedTerms]

parenthesesTests :: Test
parenthesesTests = TestList [] -- [TestCase (assertParseFail vars t) | t <- malformattedParentheses]

malformattedTermTests :: Test
malformattedTermTests = TestList [] -- [TestCase (assertParseFail vars t) | t <- malformattedTerms]

-- | Terms with imbalanced parentheses for which parsing should fail
malformattedParentheses :: [String]
malformattedParentheses =
  [ "((c)",
    "(c))",
    "f((c,y,z)",
    "f(c,y,z))",
    "f(x,) g(y))",
    "f(c,(y,z)",
    "f(c,)y,z)"
  ]

-- | Example terms which should not be parseable
malformattedTerms :: [String]
malformattedTerms =
  [ ",c",
    "c,",
    "c,y",
    "(y,z)",
    "f x"
  ]

-- | Terms which should be parseable
wellFormattedTerms :: [(String, Term String String)]
wellFormattedTerms =
  [ ("x", Var "x"),
    ("c", Fun "c" []),
    ("f(x)", Fun "f" [Var "x"]),
    ("f(x')", Fun "f" [Var "x'"]),
    ("f(x,y, z)", Fun "f" [Var "x", Var "y", Var "z"]),
    ("f(c,y,z) ", Fun "f" [Fun "c" [], Var "y", Var "z"]),
    (" f(c,f(g))", Fun "f" [Fun "c" [], Fun "f" [Fun "g" []]]),
    ("f(x,b(d,e),y)", Fun "f" [Var "x", Fun "b" [Fun "d" [], Fun "e" []], Var "y"]),
    ("f()", Fun "f" []),
    --("(x)", Var "x"),
    --("(c)", Fun "c" []),
    --("((c))", Fun "c" []),
    --("(((x)))", Var "x"),
    ("f(xy)", Fun "f" [Fun "xy" []])
  ]
