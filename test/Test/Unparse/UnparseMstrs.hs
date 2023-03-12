-- |
-- Module      : Test.Unparse.UnparseMstrs
-- Description : Unparsing tests for MSTRSs
--
-- This module defines test cases for converting many-sorted TRSRs
-- from the internal 'Mstrs' representation to COPS and ARI format.
module Test.Unparse.UnparseMstrs (unparseCopsMstrsTests, unparseAriMstrsTests) where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Conversion.Problem.Mstrs.Mstrs (Mstrs (..))
import Data.Conversion.Unparse.UnparseMstrs (unparseAriMstrs, unparseCopsMstrs)
import Prettyprinter (Pretty)
import Test.HUnit
import Test.Unparse.Utils (assertUnparseList)

-- | Unparser for COPS format
copsMstrsUnparser :: (Pretty f, Pretty v, Pretty s) => Mstrs f v s -> String
copsMstrsUnparser trs = case unparseCopsMstrs trs of
  Right unparsed -> show unparsed
  Left err -> show err -- qqjf Add error handling

-- | Tests for converting some example 'Mstrs's to COPS format
unparseCopsMstrsTests :: Test
unparseCopsMstrsTests = assertUnparseList mstrss copsMstrsUnparser
  where
    mstrss :: [(Mstrs String String String, String, String)]
    mstrss =
      [ (mstrsWithSort, "(SIG \n  (f Nat -> Nat)\n)\n(RULES \n  f(x) -> x\n)", "Unparse a COPS MSTRS with sorts specified"),
        ( mstrsCops637,
          "(SIG \n\
          \  (+ Nat Nat -> Nat)\n\
          \  (s Nat -> Nat)\n\
          \  (0 -> Nat)\n\
          \  (node Nat Tree Tree -> Tree)\n\
          \  (leaf Nat -> Tree)\n\
          \  (sum Tree -> Nat)\n\
          \)\n\
          \(RULES \n\
          \  sum(leaf(x)) -> x\n\
          \  sum(node(x,yt,zt)) -> +(+(x,sum(yt)),sum(zt))\n\
          \  +(0,y) -> y\n\
          \  +(s(x),y) -> s(+(x,y))\n\
          \  +(x,y) -> +(y,x)\n\
          \  node(x,yt,zt) -> node(x,zt,yt)\n\
          \)\n\
          \(COMMENT \n\
          \experiments for [125]\n\
          \origin: COPS #637\n\
          \submitted by: Takahito Aoto\
          \)",
          "Unparse COPS #637 MSTRS into COPS format"
        )
      ]

-- | Tests for converting some example 'Mstrs's to ARI format
unparseAriMstrsTests :: Test
unparseAriMstrsTests = assertUnparseList mstrss (show . unparseAriMstrs)
  where
    mstrss :: [(Mstrs String String String, String, String)]
    mstrss =
      [ (mstrsWithSort, "(format MSTRS)\n(sort Nat)\n(fun f :sort (Nat Nat))\n(rule (f x) x)", "Unparse ARI TRS with sorts specified"),
        ( mstrsCops637,
          "(meta-info (origin \"COPS #637\"))\n\
          \(meta-info (comment \"experiments for [125]\"))\n\
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
          "Unparse COPS #637 MSTRS into ARI format"
        )
      ]

------------------------
--- Test data ----------
------------------------

-- | An MSTRS for testing with sorts specified
mstrsWithSort :: Mstrs String String String
mstrsWithSort =
  Mstrs
    { rules = [Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}],
      signature = [MsSig "f" (["Nat"], "Nat")],
      sorts = Just ["Nat"],
      metaInfo = emptyMetaInfo
    }

-- | A more complicated example MSTRS with MetaInfo
-- Deliberately does not specify sorts to test sort inference
mstrsCops637 :: Mstrs String String String
mstrsCops637 =
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
      sorts = Nothing,
      metaInfo = emptyMetaInfo {comments = Just ["experiments for [125]"], origin = Just "COPS #637", submitted = Just ["Takahito Aoto"]}
    }
