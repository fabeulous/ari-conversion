-- |
-- Module      : Test.Parse.Mstrs.Ari
-- Description : Parsing tests for ARI MSTRSs
--
-- This module defines test cases for the function 'parseAriMstrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.Mstrs.Ari (parseAriMstrsTests) where

import Data.Conversion.Parser.Parse.ParseMstrs (parseAriMstrs)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Conversion.Problem.Mstrs.Mstrs (Mstrs (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Test cases for 'parseAriMstrs' including cases which should be parseable and cases which should fail
parseAriMstrsTests :: Test
parseAriMstrsTests = TestList [goodAriMstrsTests, badAriMstrsTests]

-- | Test cases for 'parseAriMstrs' which should succeed and match the expected output
goodAriMstrsTests :: Test
goodAriMstrsTests = TestList [TestLabel "Parse ARI MSTRSs" $ assertParseList wellFormattedTrss parseAriMstrs]
  where
    wellFormattedTrss :: [(String, Mstrs String String String)]
    wellFormattedTrss =
      [ ("(format MSTRS)", Mstrs {rules = [], signature = [], sorts = Just [], metaInfo = emptyMetaInfo}),
        ( "(meta-info (comment \"An MSTRS (with no rules)\"))(format MSTRS)\n(sort Nat)",
          Mstrs
            { rules = [],
              signature = [],
              sorts = Just ["Nat"],
              metaInfo = emptyMetaInfo {comments = Just ["An MSTRS (with no rules)"]}
            }
        ),
        ( "(format MSTRS)(sort Nat)(fun + :sort (Nat Nat Nat))(fun a :sort (Nat))(fun c :sort (Nat))(fun b :sort (Nat))(rule (+ a b) c)",
          Mstrs
            { rules = [Rule {lhs = Fun "+" [Fun "a" [], Fun "b" []], rhs = Fun "c" []}],
              signature = [MsSig "+" (["Nat", "Nat"], "Nat"), MsSig "a" ([], "Nat"), MsSig "c" ([], "Nat"), MsSig "b" ([], "Nat")],
              sorts = Just ["Nat"],
              metaInfo = emptyMetaInfo
            }
        ),
        ( "(meta-info (comment \"experiments for [125]\"))\n\
          \(meta-info (origin \"COPS #637\"))\n\
          \(meta-info (submitted \"Takahito Aoto\"))\n\
          \(format MSTRS)\n\
          \(sort Nat)\n\
          \(sort Tree)\n\
          \(fun + :sort (Nat Nat Nat))\n\
          \(fun s :sort (Nat Nat))\n\
          \(fun 0 :sort (Nat))\n\
          \(fun node :sort (Nat Tree Tree Tree))\n\
          \(fun leaf :sort (Nat Tree))\n\
          \(fun sum :sort (Tree Nat))\n\
          \(rule (sum (leaf x)) x)\n\
          \(rule (sum (node x yt zt)) (+ (+ x (sum yt)) (sum zt)))\n\
          \(rule (+ 0 y) y)\n\
          \(rule (+ (s x) y) (s (+ x y)))\n\
          \(rule (+ x y) (+ y x))\n\
          \(rule (node x yt zt) (node x zt yt))",
          Mstrs
            { rules =
                [ Rule {lhs = Fun "sum" [Fun "leaf" [Var "x"]], rhs = Var "x"},
                  Rule {lhs = Fun "sum" [Fun "node" [Var "x", Var "yt", Var "zt"]], rhs = Fun "+" [Fun "+" [Var "x", Fun "sum" [Var "yt"]], Fun "sum" [Var "zt"]]},
                  Rule {lhs = Fun "+" [Fun "0" [], Var "y"], rhs = Var "y"},
                  Rule {lhs = Fun "+" [Fun "s" [Var "x"], Var "y"], rhs = Fun "s" [Fun "+" [Var "x", Var "y"]]},
                  Rule {lhs = Fun "+" [Var "x", Var "y"], rhs = Fun "+" [Var "y", Var "x"]},
                  Rule {lhs = Fun "node" [Var "x", Var "yt", Var "zt"], rhs = Fun "node" [Var "x", Var "zt", Var "yt"]}
                ],
              signature =
                [ MsSig "+" (["Nat", "Nat"], "Nat"),
                  MsSig "s" (["Nat"], "Nat"),
                  MsSig "0" ([], "Nat"),
                  MsSig "node" (["Nat", "Tree", "Tree"], "Tree"),
                  MsSig "leaf" (["Nat"], "Tree"),
                  MsSig "sum" (["Tree"], "Nat")
                ],
              sorts = Just ["Nat", "Tree"],
              metaInfo =
                emptyMetaInfo
                  { comments = Just ["experiments for [125]"],
                    doi = Nothing,
                    origin = Just "COPS #637",
                    submitted = Just ["Takahito Aoto"]
                  }
            }
        )
      ]

-- | Malformatted examples for which it is asserted that 'parseAriMstrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badAriMstrsTests :: Test
badAriMstrsTests = TestList [TestLabel "parseAriMstrs should fail" $ assertFailParseList badTrss parseAriMstrs]
  where
    badTrss :: [String]
    badTrss =
      [ "(sort Nat)(fun a :sort (Nat))(fun b :sort (Nat))(rule a b)", -- No format
        "(format TRS)(sort Nat)(fun a :sort (Nat))(fun b :sort (Nat))(rule a b)", -- Wrong format
        "(format TRS)(fun a :sort (Nat))(fun b :sort (Nat))(sort Nat)(rule a b)" -- sort defined after signature
      ]
