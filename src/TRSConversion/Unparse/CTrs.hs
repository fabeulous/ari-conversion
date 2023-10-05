{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Unparse.CTrs
Description : Unparser for TRSs

This module defines functions to output a 'CTrs' in COPS and ARI format.
-}
module TRSConversion.Unparse.CTrs (
  -- * COPS
  unparseCopsCTrs,

  -- ** Helpers
  prettyCondType,
  prettyCRule,
  unparseCondition,

  -- * ARI
  unparseAriCTrs,
  unparseAriCSystems,
  unparseAriCRules,
  prettyAriConditionType,
  unparseAriCTrsSig,
  unparseAriCondition,
)
where

import Data.Containers.ListUtils (nubOrd)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Prettyprinter (Doc, Pretty, concatWith, hardline, hsep, nest, parens, pretty, space, vsep, (<+>))
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..), inferSigFromRules)
import TRSConversion.Problem.Common.Term (vars)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))
import TRSConversion.Unparse.Problem.Term (unparsePrefixTerm, unparseTerm)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)

{- | Unparse a first-order TRS from the Haskell 'Trs' representation into
[COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).

Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
unparse each part of the 'Trs'.

See the tests for examples of expected output.
-}
unparseCopsCTrs :: (Ord v, Pretty f, Pretty v) => CTrs f v -> Either String (Doc ann)
unparseCopsCTrs ctrs
  | numSystems ctrs /= 1 = error "COPS format doesn't support CTRSs with multiple systems"
  | otherwise = do
    pure $
      vsep
        [ prettyBlock "CONDITIONTYPE" (prettyCondType $ conditionType ctrs)
        , prettyBlock "VAR" (hsep [pretty v | v <- variables])
        , prettyBlock "RULES" (nest 2 (vsep $ mempty : [prettyCRule r | r <- rs]) <> hardline)
        ]
 where
  rs = rules ctrs IntMap.! 1
  variables = varsOfTrs rs

  varsOfTrs rls = nubOrd $ concatMap varsOfRules rls

  varsOfRules r = vars (lhs r) ++ vars (rhs r)

prettyCRule :: (Pretty f, Pretty v) => CRule f v -> Doc ann
prettyCRule cRule =
  unparseTerm (lhs cRule) <+> "->" <+> unparseTerm (rhs cRule) <> unparseConditions (conditions cRule)

unparseConditions :: (Pretty f, Pretty v) => [Condition f v] -> Doc ann
unparseConditions [] = mempty
unparseConditions cnds = space <> "|" <+> concatWith (\d1 d2 -> d1 <> "," <+> d2) (map unparseCondition cnds)

unparseCondition :: (Pretty f, Pretty v) => Condition f v -> Doc ann
unparseCondition (t1 :== t2) = unparseTerm t1 <+> "==" <+> unparseTerm t2

prettyCondType :: CondType -> Doc ann
prettyCondType SemiEquational = "SEMI-EQUATIONAL"
prettyCondType Join = "JOIN"
prettyCondType Oriented = "ORIENTED"

unparseAriCTrs :: (Pretty f, Pretty v) =>CTrs f v -> Either String (Doc ann)
unparseAriCTrs ctrs = do
  ariSig <- unparseAriCTrsSig (signature ctrs)
  let trsElements =
        [ prettyAriFormat (conditionType ctrs)
        , ariSig
        , unparseAriCSystems (rules ctrs)
        ]
  return $ vsep (filterEmptyDocs trsElements)

unparseAriCTrsSig :: (Pretty f) => TrsSig f v -> Either String (Doc ann)
unparseAriCTrsSig (FunSig fs) = Right (vsep $ map (prettyBlock "fun" . pretty) fs)

unparseAriCSystems :: (Pretty f, Pretty v) => IntMap [CRule f v] -> Doc ann
unparseAriCSystems systems = vsep $ fmap (uncurry unparseAriCRules) (IntMap.toList systems)

unparseAriCRules :: (Pretty f, Pretty v) => Int -> [CRule f v] -> Doc ann
unparseAriCRules index = vsep . map (unparseCRule index)

unparseCRule :: (Pretty f, Pretty v) => Int -> CRule f v -> Doc ann
unparseCRule index (CRule{lhs = l, rhs = r, conditions = cnds}) =
  parens $ "rule" <+> unparsePrefixTerm l <+> unparsePrefixTerm r
    <> conds cnds <> optIndex
 where
  conds [] = mempty
  conds cs = space <> hsep (map unparseAriCondition cs)

  optIndex
    | index == 1 = mempty
    | otherwise = mempty <+> ":index" <+> pretty index

unparseAriCondition :: (Pretty f, Pretty v) => Condition f v -> Doc ann
unparseAriCondition (t1 :== t2) = parens $ "=" <+> unparsePrefixTerm t1 <+> unparsePrefixTerm t2

prettyAriFormat :: CondType -> Doc ann
prettyAriFormat condType = parens $ "format CTRS" <+> prettyAriConditionType condType

prettyAriConditionType :: CondType -> Doc ann
prettyAriConditionType SemiEquational = "semi-equational"
prettyAriConditionType Join = "join"
prettyAriConditionType Oriented = "oriented"
