-- |
-- Module      : Test.Unparse.UnparseMstrs
-- Description : Unparsing tests for MSTRSs
--
-- This module defines test cases for converting many-sorted TRSRs
-- from the internal 'Mstrs' representation to COPS and ARI format.
module Test.Unparse.UnparseMstrs (unparseCopsMstrsTests, unparseAriMstrsTests) where

import Data.Conversion.Parser.Unparse.UnparseMstrs (unparseAriMstrs, unparseCopsMstrs)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Conversion.Problem.Mstrs.Mstrs (Mstrs (..))
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
      [ (mstrsVarSig, "(SIG \n  (f Nat -> Nat)\n)\n(RULES \n  f(x) -> x\n)", "Unparse a COPS MSTRS with sorts specified")
      ]

-- | Tests for converting some example 'Mstrs's to ARI format
unparseAriMstrsTests :: Test
unparseAriMstrsTests = assertUnparseList mstrss (show . unparseAriMstrs)
  where
    mstrss :: [(Mstrs String String String, String, String)]
    mstrss =
      [ (mstrsVarSig, "(format MSTRS)\n(sort Nat)\n(fun f :sort (Nat Nat))\n(rule (f x) x)", "Unparse ARI TRS with sorts specified")
      ]

------------------------
--- Test data ----------
------------------------

-- | A TRS for testing with sorts specified
mstrsVarSig :: Mstrs String String String
mstrsVarSig =
  Mstrs
    { rules = [Rule {lhs = Fun "f" [Var "x"], rhs = Var "x"}],
      signature = [MsSig "f" (["Nat"], "Nat")],
      sorts = Just ["Nat"],
      metaInfo = emptyMetaInfo
    }