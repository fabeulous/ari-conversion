{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Unparse.ARI.CTrs
Description : Unparser for TRSs

This module defines functions to output a 'CTrs' in COPS and ARI format.
-}
module TRSConversion.Unparse.ARI.CTrs (
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
import TRSConversion.Unparse.ARI.Problem.Term (unparsePrefixTerm)
import TRSConversion.Unparse.ARI.Utils (filterEmptyDocs, prettyBlock)

unparseAriCTrs :: (Pretty f, Pretty v) =>CTrs f v -> Either String (Doc ann)
unparseAriCTrs ctrs = do
  ariSig <- unparseAriCTrsSig (signature ctrs)
  let trsElements =
        [ prettyAriFormat (conditionType ctrs)
        , ariSig
        , unparseAriCSystems (rules ctrs)
        ]
  return $ vsep (filterEmptyDocs trsElements)

unparseAriCTrsSig :: (Pretty f) => TrsSig f -> Either String (Doc ann)
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
