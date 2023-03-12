-- |
-- Module      : Test.Parse.Trs.Ari
-- Description : Parsing tests for ARI TRSs
--
-- This module defines test cases for the function 'parseAriTrs'.
-- It is non-exhaustive, but intended to highlight any obvious errors.
module Test.Parse.Trs.Ari (parseAriTests) where

import Data.Conversion.Parse.ParseTrs (parseAriTrs)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

-- | Test cases for 'parseAriTrs' including cases which should be parseable and cases which should fail
parseAriTests :: Test
parseAriTests = TestLabel "parseAriTests" $ TestList [parseAriTrsTests, badAriTrsTests]

-- | Test cases for 'parseAriTrs' which should succeed and match the expected output
parseAriTrsTests :: Test
parseAriTrsTests = assertParseList "parseAriTrs should succeed" wellFormattedTrss parseAriTrs
  where
    wellFormattedTrss :: [(String, Trs String String)]
    wellFormattedTrss =
      [ ( "(meta-info (comment \"A simple TRS\")) \
          \ (format TRS)\
          \ (fun f 1)\
          \ (rule (f x) (x))",
          Trs
            { rules = [Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}],
              signature = FunSig [Sig "f" 1],
              metaInfo = emptyMetaInfo {comments = Just ["A simple TRS"]}
            }
        ),
        ( "(format TRS)", -- Minimal definition
          Trs
            { rules = [],
              signature = FunSig [],
              metaInfo = emptyMetaInfo
            }
        ),
        ( "(meta-info (origin \"COPS #20\")) \
          \ (meta-info (doi \"10.1007/11805618_6\")) \
          \ (meta-info (comment \"[7] Example 2\")) \
          \ (meta-info (submitted \"Takahito Aoto\" \"Junichi Yoshida\" \"Yoshihito Toyama\")) \
          \ (format TRS) \
          \ (fun 0 0) \
          \ (fun nats 0) \
          \ (fun hd 1) \
          \ (fun s 1) \
          \ (fun tl 1) \
          \ (fun inc 1) \
          \ (fun : 2) \
          \ (rule nats (: 0 (inc nats))) \
          \ (rule (inc (: x y)) (: (s x) (inc y))) \
          \ (rule (hd (: x y)) x) \
          \ (rule (tl (: x y)) x) \
          \ (rule (inc (tl nats)) (tl (inc nats)))",
          Trs
            { rules =
                [ Rule {lhs = Fun "nats" [], rhs = Fun ":" [Fun "0" [], Fun "inc" [Fun "nats" []]]},
                  Rule {lhs = Fun "inc" [Fun ":" [Var "x", Var "y"]], rhs = Fun ":" [Fun "s" [Var "x"], Fun "inc" [Var "y"]]},
                  Rule {lhs = Fun "hd" [Fun ":" [Var "x", Var "y"]], rhs = Var "x"},
                  Rule {lhs = Fun "tl" [Fun ":" [Var "x", Var "y"]], rhs = Var "x"},
                  Rule {lhs = Fun "inc" [Fun "tl" [Fun "nats" []]], rhs = Fun "tl" [Fun "inc" [Fun "nats" []]]}
                ],
              signature = FunSig [Sig "0" 0, Sig "nats" 0, Sig "hd" 1, Sig "s" 1, Sig "tl" 1, Sig "inc" 1, Sig ":" 2],
              metaInfo =
                emptyMetaInfo
                  { comments = Just ["[7] Example 2"],
                    doi = Just "10.1007/11805618_6",
                    origin = Just "COPS #20",
                    submitted = Just ["Takahito Aoto", "Junichi Yoshida", "Yoshihito Toyama"]
                  }
            }
        )
      ]

-- | Malformatted examples for which it is asserted that 'parseAriTrs' should not succeed.
-- This list is non-exhaustive, but checks for some common problems.
badAriTrsTests :: Test
badAriTrsTests = assertFailParseList "parseAriTrs should fail" badTrss parseAriTrs
  where
    badTrss :: [String]
    badTrss =
      [ "(fun f 1) \n (rule (f x) x)", -- No format given
        "(format MSTRS)(fun f 1)(rule (f x) (x))", -- Wrong format
        "(format TRS)\n(fun f 1)\n (rule (f x) x)\n(fun g 1) ", -- fun block after rule block
        "(rule (f x) x)\n(format TRS)\n(fun f 1)", -- rule before format
        "(format TRS)\n(fun f 1)\n(rule (g x) x)", -- function symbol g not in signature
        "(VAR x)(RULES f(x)->x)", -- COPS format
        "(format TRS)\n(fun f 1)\n(rule (f x) x)(meta-info (comment \"Some comment\"))" -- meta-info at end
      ]
