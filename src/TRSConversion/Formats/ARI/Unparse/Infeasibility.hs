{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Unparse.ARI.Infeasibility (
    unparseAriInfeasibility,
) where

import Prettyprinter (Doc, Pretty, hsep, parens, vsep, (<+>))

import TRSConversion.Problem.CTrs.CTrs (CTrs (..), Condition (..), conditionType, rules, signature)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import qualified TRSConversion.Problem.CTrs.Infeasibility as Inf
import TRSConversion.Unparse.ARI.CTrs (prettyAriConditionType, unparseAriCSystems, unparseAriCTrsSig, unparseAriCondition)
import TRSConversion.Unparse.Utils (filterEmptyDocs)

unparseAriInfeasibility :: (Pretty f, Pretty v) => Infeasibility f v -> Either String (Doc ann)
unparseAriInfeasibility infProb = do
    let system = Inf.ctrs infProb
    ariSig <- unparseAriCTrsSig (signature system)
    let trsElements =
            [ prettyAriInfFormat infProb
            , ariSig
            , unparseAriCSystems (rules system)
            , unparseAriQuery (Inf.query infProb)
            ]
    return $ vsep (filterEmptyDocs trsElements)

unparseAriQuery :: (Pretty f, Pretty v) => [Condition f v] -> Doc ann
unparseAriQuery conds = parens $ "infeasible?" <+> hsep (map unparseAriCondition conds)

prettyAriInfFormat :: Infeasibility f v -> Doc ann
prettyAriInfFormat Infeasibility { isTrs = sysIsTrs, ctrs = CTrs {conditionType = condType}}
  --  | condType == Oriented && all (all (null . conditions)) rs =
  | sysIsTrs = parens $ "format TRS" <+> ":problem" <+> "infeasibility"
  | otherwise = parens $ "format CTRS" <+> prettyAriConditionType condType <+> ":problem" <+> "infeasibility"
