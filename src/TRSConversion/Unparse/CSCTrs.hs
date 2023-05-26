{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Unparse.CSCTrs
Description : Unparser for CSCTRSs
-}
module TRSConversion.Unparse.CSCTrs (
  -- * COPS
  unparseCopsCSCTrs,

  -- * ARI
  unparseAriCSCTrs,
)
where

import Data.List (group, sort)
import Prettyprinter (Doc, Pretty, hardline, hsep, nest, parens, pretty, vsep, (<+>))
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), inferSigFromRules)
import TRSConversion.Problem.Common.Term (vars)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..), Sig (..))
import TRSConversion.Unparse.CSTrs (copsReplacementMap)
import TRSConversion.Unparse.CTrs (prettyCRule, prettyCondType, prettyAriConditionType, unparseAriCRules)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import TRSConversion.Problem.CSTrs.CSTrs (ReplacementMap)
import qualified Data.Map.Strict as M

{- | Unparse a first-order TRS from the Haskell 'Trs' representation into
[COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).

Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
unparse each part of the 'Trs'.

See the tests for examples of expected output.
-}
unparseCopsCSCTrs :: (Ord v, Pretty f, Pretty v) => CSCTrs f v -> Either String (Doc ann)
unparseCopsCSCTrs CSCTrs{ctrs = ctrs, replacementMap = repMap} = do
  pure $
    vsep
      [ prettyBlock "CONDITIONTYPE" (prettyCondType $ conditionType ctrs)
      , prettyBlock "VAR" (hsep [pretty v | v <- vs])
      , prettyBlock "REPLACEMENT-MAP" (nest 2 (hardline <> copsReplacementMap repMap) <> hardline)
      , prettyBlock "RULES" (nest 2 (vsep $ mempty : [prettyCRule r | r <- rules ctrs]) <> hardline)
      ]
 where
  vs = varsOfTrs (signature ctrs) (rules ctrs)

  varsOfTrs (Vars vas) _ = vas
  varsOfTrs _ rs = map head . group . sort $ concatMap varsOfRules rs

  varsOfRules r = vars (lhs r) ++ vars (rhs r)

unparseAriCSCTrs :: (Pretty f, Pretty v, Ord f) =>CSCTrs f v -> Either String (Doc ann)
unparseAriCSCTrs CSCTrs{ctrs = ctrs, replacementMap = repMap} = do
  ariSig <- unparseAriCSCTrsSig (rules ctrs) (signature ctrs) repMap
  let trsElements =
        [ parens $ "format CSCTRS" <+> prettyAriConditionType (conditionType ctrs)
        , ariSig
        , unparseAriCRules (rules ctrs)
        ]
  return $ vsep (filterEmptyDocs trsElements)

unparseAriCSCTrsSig :: (Ord f, Pretty f) => [CRule f v] -> TrsSig f v -> ReplacementMap f -> Either String (Doc ann)
unparseAriCSCTrsSig rs sig repMap = go sig
 where
  repMapM = M.fromList repMap
  prettyM = M.map (\ints -> mempty <+> ":replacement-map" <+> (parens . hsep $ map pretty ints)) repMapM

  go (FunSig fs) = Right (vsep $ map prettySigLine fs)
  go (FullSig _ fs) = Right (vsep $ map prettySigLine fs)
  go (Vars _) = go . FunSig =<< inferSigFromRules rs

  prettySigLine (Sig f a) = parens $ "fun" <+> pretty f <+> pretty a <> M.findWithDefault mempty f prettyM

