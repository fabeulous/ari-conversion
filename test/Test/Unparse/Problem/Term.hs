-- |
-- Module      : Test.Unparse.Problem.Term
-- Description : Unparsing tests for terms
--
-- This module defines test cases for unparsing 'Term's to applicative and
-- prefix format.
module Test.Unparse.Problem.Term (unparseTermTests) where

import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Unparse.Problem.Term (unparsePrefixTerm, unparseTerm)
import Test.HUnit
import Test.Unparse.Utils (assertUnparseList)

-- | Tests for unparsing 'Term's into COPS format and ARI format
unparseTermTests :: Test
unparseTermTests = TestLabel "unparseTermTests" $ TestList [unparseAppTermTests, unparsePrefixTermTests]

-- | Tests for converting some example 'Term's to applicative notation using 'unparseTerm'
unparseAppTermTests :: Test
unparseAppTermTests = assertUnparseList testTerms (Right . unparseTerm)
  where
    testTerms :: [(Term String String, String, String)]
    testTerms = [(t, expected, label ++ " [applicative]") | (label, t, expected, _) <- exampleTerms]

-- | Tests for converting some example 'Term's to prefix notation using 'unparsePrefixTerm'
unparsePrefixTermTests :: Test
unparsePrefixTermTests = assertUnparseList testTerms (Right . unparsePrefixTerm)
  where
    testTerms :: [(Term String String, String, String)]
    testTerms = [(t, expected, label ++ " [prefix]") | (label, t, _, expected) <- exampleTerms]

------------------------
--- Test data ----------
------------------------

-- | Example terms for testing.
-- Consists of tuples @(test label, term, applicative format, prefix format)@
exampleTerms :: [(String, Term String String, String, String)]
exampleTerms =
  [ ("Unparse variable", Var "x", "x", "x"),
    ("Unparse unary finction", Fun "fun" [Var "x"], "fun(x)", "fun x"),
    ("Unparse constant", Fun "c" [], "c", "c"),
    ("Unparse non-linear term", Fun "fun" [Var "xs", Var "xs"], "fun(xs,xs)", "fun xs xs"),
    ("Unparse nested constant", Fun "f" [Var "x", Fun "f2" [Var "y", Fun "0" []]], "f(x,f2(y,0))", "f x (f2 y 0)"),
    ("Unparse nested unary functions", Fun "f" [Fun "g" [Fun "h" [Fun "0" []]]], "f(g(h(0)))", "f (g (h 0))"),
    ("Unparse nested function application", Fun "f" [Var "x", Fun "g" [Var "y"], Var "z"], "f(x,g(y),z)", "f x (g y) z"),
    ("Unparse repeated function symbol", Fun "f" [Fun "+" [Var "1", Fun "+" [Var "1", Var "2"]], Var "3"], "f(+(1,+(1,2)),3)", "f (+ 1 (+ 1 2)) 3")
  ]