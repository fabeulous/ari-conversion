-- |
-- Module      : Test.Parse.Trs.Cops
-- Description : Parsing tests for COPS TRSs
--
-- This module defines test cases for the function 'parseCops'. It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.Trs.Cops (parseCopsTests) where

import Data.Conversion.Parser.Parse.Cops (parseCops)
import Data.Conversion.Problem.Rule (Rule (..))
import Data.Conversion.Problem.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Test cases for 'parseCops' including cases which should be parseable and cases which should fail
parseCopsTests :: Test
parseCopsTests = TestList [parseTrsTests, badTrsTests]

-- | Test cases for 'parseCops' which should succeed and match the expected output
parseTrsTests :: Test
parseTrsTests = assertParseList wellFormattedTrss parseCops
  where
    wellFormattedTrss :: [(String, Trs String String)]
    wellFormattedTrss =
      [ ( "(VAR x y)(RULES f(x,y)->g(c))",
          Trs
            { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Fun "g" [Fun "c" []]}],
              variables = ["x", "y"],
              signature = [Sig "f" 2, Sig "g" 1, Sig "c" 0],
              comment = Nothing
            }
        ),
        ( " (VAR x y)\n(SIG (f 2) (a 0) (b 1))(RULES f(x,y)->y)  (COMMENT A TRS (with SIG given)) ",
          Trs
            { rules = [Rule {lhs = Fun "f" [Var "x", Var "y"], rhs = Var "y"}],
              variables = ["x", "y"],
              signature = [Sig "f" 2, Sig "a" 0, Sig "b" 1],
              comment = Just "A TRS (with SIG given)"
            }
        ),
        ( "(RULES f(x)->x)", -- A TRS is ground if no VAR block is given
          Trs
            { rules = [Rule {lhs = Fun "f" [Fun "x" []], rhs = Fun "x" []}],
              variables = [],
              signature = [Sig "f" 1, Sig "x" 0],
              comment = Nothing
            }
        )
      ]

-- | Malformatted examples for which it is asserted that 'parseCops' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badTrsTests :: Test
badTrsTests = assertFailParseList badTrss parseCops
  where
    badTrss :: [String]
    badTrss =
      [ "(VAR x y) (RULES f(x)->y \n f(x,y)->y )", -- Mixed arities of f
        "(VAR x y) (RULES f(x,y)->f(x) \n f(x,y)->y )", -- Mixed arities of f
        "(RULES f(x)->y) (VAR x y) ", -- RULES before VARS
        "(VAR x)\n(SIG (f 1))(RULES f(x)->g(x))", -- SIG and VARS should contain all symbols
        "(SIG (f 1))(VAR x)(RULES f(x)->x)", -- SIG before VARS
        "(COMMENT (f 1))(VAR x)(RULES f(x)-x)", -- COMMENT at start
        "(VAR f x)(SIG (f 1 g 1))(RULES g(x)->x)" -- f in VAR and SIG
      ]
