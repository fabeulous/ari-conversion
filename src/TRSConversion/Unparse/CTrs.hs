{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Unparse.CTrs
Description : Unparser for TRSs

This module defines functions to output a 'CTrs' in COPS and ARI format.
-}
module TRSConversion.Unparse.CTrs (
  -- * COPS
  unparseCopsCTrs,

  -- * ARI
  unparseAriCTrs,
)
where

import Data.List (group, sort)
import Prettyprinter (Doc, Pretty, concatWith, hardline, hsep, nest, parens, pretty, space, vsep, (<+>))
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..), inferSigFromRules)
import TRSConversion.Problem.Common.Term (vars)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))
import TRSConversion.Unparse.Problem.Rule (parensTerm)
import TRSConversion.Unparse.Problem.Term (unparseTerm)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)

{- | Unparse a first-order TRS from the Haskell 'Trs' representation into
[COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).

Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
unparse each part of the 'Trs'.

See the tests for examples of expected output.
-}
unparseCopsCTrs :: (Ord v, Pretty f, Pretty v) => CTrs f v -> Either String (Doc ann)
unparseCopsCTrs ctrs = do
  pure $
    vsep
      [ prettyBlock "CONDITIONTYPE" (prettyCondType $ conditionType ctrs)
      , prettyBlock "VAR" (hsep [pretty v | v <- vs])
      , prettyBlock "RULES" (nest 2 (vsep $ mempty : [prettyCRule r | r <- rules ctrs]) <> hardline)
      ]
 where
  vs = varsOfTrs (signature ctrs) (rules ctrs)

  varsOfTrs (Vars vas) _ = vas
  varsOfTrs _ rs = map head . group . sort $ concatMap varsOfRules rs

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

unparseAriCTrs :: (Ord v, Pretty f, Pretty v, Ord f) => CTrs f v -> Either String (Doc ann)
unparseAriCTrs ctrs = do
  ariSig <- unparseAriCTrsSig (rules ctrs) (signature ctrs)
  let trsElements =
        [ prettyAriFormat (conditionType ctrs)
        , ariSig
        , unparseAriCRules (rules ctrs)
        ]
  return $ vsep (filterEmptyDocs trsElements)

unparseAriCTrsSig :: (Eq v, Ord f, Pretty f, Pretty v) => [CRule f v] -> TrsSig f v -> Either String (Doc ann)
unparseAriCTrsSig _ (FunSig fs) = Right (vsep $ map (prettyBlock "fun" . pretty) fs)
unparseAriCTrsSig rs (FullSig _ fs) = unparseAriCTrsSig rs (FunSig fs)
unparseAriCTrsSig rs (Vars _) = case inferSigFromRules rs of -- Extract signature from TRS rules
  Right fs -> unparseAriCTrsSig rs (FunSig fs)
  Left err -> Left err

unparseAriCRules :: (Pretty f, Pretty v) => [CRule f v] -> Doc ann
unparseAriCRules = vsep . map unparseCRule

unparseCRule :: (Pretty f, Pretty v) => CRule f v -> Doc ann
unparseCRule (CRule{lhs = l, rhs = r, conditions = cnds}) =
  parens $ "rule" <+> parensTerm l <+> parensTerm r <> conds cnds
 where
  conds [] = mempty
  conds cs = space <> ":condition" <+> parens (hsep (map unparseCond cs))

unparseCond :: (Pretty f, Pretty v) => Condition f v -> Doc ann
unparseCond (t1 :== t2) = parens $ "=" <+> parensTerm t1 <+> parensTerm t2

prettyAriFormat :: CondType -> Doc ann
prettyAriFormat SemiEquational = "(format CTRS semi-equational)"
prettyAriFormat Join = "(format CTRS join)"
prettyAriFormat Oriented = "(format CTRS oriented)"
