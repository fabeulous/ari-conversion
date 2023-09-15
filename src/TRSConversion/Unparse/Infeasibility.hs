{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Unparse.Infeasibility (
    unparseAriInfeasibility,
    unparseCopsInfeasibility,
) where

import Prettyprinter (Doc, Pretty, hsep, parens, pretty, vsep, (<+>), concatWith)
import TRSConversion.Problem.CTrs.CTrs (CondType, Condition (..), conditionType, rules, signature, CRule (CRule), varsCondition)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (query))
import qualified TRSConversion.Problem.CTrs.Infeasibility as Inf
import TRSConversion.Unparse.CTrs (prettyAriConditionType, unparseAriCSystems, unparseAriCTrsSig, unparseAriCondition, unparseCopsCTrs, unparseCondition)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Data.Containers.ListUtils (nubOrd)

unparseCopsInfeasibility :: (Ord v, Pretty f, Pretty v) => String -> Infeasibility f v -> Either String (Doc ann)
unparseCopsInfeasibility comment inf = do
    prettySystems <- unparseCopsCTrs (Inf.ctrs inf)
    pure $
        vsep
            [ parens $ "PROBLEM" <+> "COMMUTATION"
            , parens $ "COMMENT" <+> pretty comment
            , prettySystems
            , prettyBlock "VAR" $ hsep $ map pretty $ nubOrd $ concatMap varsCondition (query inf)
            , parens $ "CONDITION" <+> concatWith (\l r -> l <> "," <+> r) [unparseCondition c | c <- query inf]
            ]

unparseAriInfeasibility :: (Ord v, Pretty f, Pretty v, Ord f) => Infeasibility f v -> Either String (Doc ann)
unparseAriInfeasibility infProb = do
    let ctrs = Inf.ctrs infProb
    ariSig <- unparseAriCTrsSig (map queryToRule (query infProb) ++ concat (rules ctrs)) (signature ctrs)
    let trsElements =
            [ prettyAriInfFormat (conditionType ctrs)
            , ariSig
            , unparseAriCSystems (rules ctrs)
            , unparseAriQuery (Inf.query infProb)
            ]
    return $ vsep (filterEmptyDocs trsElements)
  where
    queryToRule (l :== r) = CRule l r []

unparseAriQuery :: (Pretty f, Pretty v) => [Condition f v] -> Doc ann
unparseAriQuery conds = parens $ "infeasible?" <+> hsep (map unparseAriCondition conds)

prettyAriInfFormat :: CondType -> Doc ann
prettyAriInfFormat condType = parens $ "format CTRS" <+> prettyAriConditionType condType <+> ":problem" <+> "infeasibility"
