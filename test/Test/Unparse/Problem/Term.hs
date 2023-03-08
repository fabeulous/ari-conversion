-- |
-- Module      : Test.Unparse.Problem.Term
-- Description : Unparsing tests for terms
--
-- This module defines test cases for unparsing 'Term's to applicative and
-- prefix format.
module Test.Unparse.Problem.Term (unparseTermTests) where

import Data.Conversion.Parser.Unparse.Problem.Term (unparsePrefixTerm, unparseTerm)
import Data.Conversion.Problem.Common.Term (Term (..))
import Prettyprinter (Pretty)
import Test.HUnit
import Test.Unparse.Utils (assertUnparseList)

unparseTermTests :: Test
unparseTermTests = TestList [unparseAppTermTests, unparsePrefixTermTests]

-- | Unparser for applicative term. 'show's the result of 'unparseTerm'.
appTermUnparser :: (Pretty f, Pretty v) => Term f v -> String
appTermUnparser t = show $ unparseTerm t

-- | Unparser for prefix terms. 'show's the result of '
prefixTermUnparser :: (Pretty f, Pretty v) => Term f v -> String
prefixTermUnparser t = show $ unparsePrefixTerm t

-- | Tests for converting some example 'Term's to applicative format
unparseAppTermTests :: Test
unparseAppTermTests = assertUnparseList testTerms appTermUnparser
  where
    testTerms :: [(Term String String, String)]
    testTerms = [(t, expected) | (t, expected, _) <- exampleTerms]

unparsePrefixTermTests :: Test
unparsePrefixTermTests = assertUnparseList testTerms prefixTermUnparser
  where
    testTerms :: [(Term String String, String)]
    testTerms = [(t, expected) | (t, _, expected) <- exampleTerms]

------------------------
--- Test data ----------
------------------------

-- | Example terms for testing.
-- Consists of tuples @(term, applicative format, prefix format)@
exampleTerms :: [(Term String String, String, String)]
exampleTerms =
  [ (Var "x", "x", "x"),
    (Fun "fun" [Var "x"], "fun(x)", "fun x"),
    (Fun "c" [], "c", "c"),
    (Fun "fun" [Var "xs", Var "xs"], "fun(xs,xs)", "fun xs xs"),
    (Fun "f" [Var "x", Fun "f2" [Var "y", Fun "0" []]], "f(x,f2(y,0))", "f x (f2 y 0)"),
    (Fun "f" [Fun "+" [Var "1", Fun "+" [Var "1", Var "2"]], Var "3"], "f(+(1,+(1,2)),3)", "f (+ 1 (+ 1 2)) 3"),
    (Fun "f" [Fun "g" [Fun "h" [Fun "0" []]]], "f(g(h(0)))", "f g h 0")
  ]