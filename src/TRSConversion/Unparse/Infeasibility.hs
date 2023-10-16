{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Unparse.Infeasibility (
    unparseAriInfeasibility,
    unparseCopsInfeasibility,
) where

import Prettyprinter (Doc, Pretty, hsep, parens, pretty, vsep, (<+>), concatWith)
import TRSConversion.Problem.CTrs.CTrs (CondType (Oriented), Condition (..), conditionType, rules, signature, varsCondition, CTrs (..), CRule (conditions), orientedCTrsToTrs)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (query))
import qualified TRSConversion.Problem.CTrs.Infeasibility as Inf
import TRSConversion.Unparse.CTrs (prettyAriConditionType, unparseAriCSystems, unparseAriCTrsSig, unparseAriCondition, unparseCopsCTrs, unparseCondition)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Data.Containers.ListUtils (nubOrd)
import TRSConversion.Unparse.UnparseTrs (unparseCopsTrs)

unparseCopsInfeasibility :: (Ord v, Pretty f, Pretty v, Ord f) => String -> Infeasibility f v -> Either String (Doc ann)
unparseCopsInfeasibility comment inf = do
    prettySystems <-
      case orientedCTrsToTrs (Inf.ctrs inf) of
        Nothing -> unparseCopsCTrs (Inf.ctrs inf)
        Just trs -> unparseCopsTrs trs
    pure $
        vsep
            [ parens $ "PROBLEM" <+> "COMMUTATION"
            , parens $ "COMMENT" <+> pretty comment
            , prettySystems
            , prettyBlock "VAR" $ hsep $ map pretty $ nubOrd $ concatMap varsCondition (query inf)
            , parens $ "CONDITION" <+> concatWith (\l r -> l <> "," <+> r) [unparseCondition c | c <- query inf]
            ]

unparseAriInfeasibility :: (Pretty f, Pretty v) => Infeasibility f v -> Either String (Doc ann)
unparseAriInfeasibility infProb = do
    let ctrs = Inf.ctrs infProb
    ariSig <- unparseAriCTrsSig (signature ctrs)
    let trsElements =
            [ prettyAriInfFormat ctrs
            , ariSig
            , unparseAriCSystems (rules ctrs)
            , unparseAriQuery (Inf.query infProb)
            ]
    return $ vsep (filterEmptyDocs trsElements)

unparseAriQuery :: (Pretty f, Pretty v) => [Condition f v] -> Doc ann
unparseAriQuery conds = parens $ "infeasible?" <+> hsep (map unparseAriCondition conds)

prettyAriInfFormat :: CTrs f v -> Doc ann
prettyAriInfFormat CTrs {conditionType = condType, rules = rs}
  | condType == Oriented && all (all (null . conditions)) rs =
      parens $ "format TRS" <+> ":problem" <+> "infeasibility"
  | otherwise = parens $ "format CTRS" <+> prettyAriConditionType condType <+> ":problem" <+> "infeasibility"
