{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Formats.COPS.Unparse.Infeasibility (
    unparseCopsInfeasibility,
) where

import Data.Containers.ListUtils (nubOrd)
import Prettyprinter (Doc, Pretty, concatWith, hsep, parens, pretty, vsep, (<+>))

import TRSConversion.Formats.COPS.Unparse.CTrs (unparseCondition, unparseCopsCTrs, unparseCopsCTrsSystem)
import TRSConversion.Problem.CTrs.CTrs (orientedCTrsToTrs, varsCondition)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import qualified TRSConversion.Problem.CTrs.Infeasibility as Inf
import TRSConversion.Unparse.Utils (prettyBlock)

unparseCopsInfeasibility :: (Ord v, Pretty f, Pretty v) => String -> Infeasibility f v -> Either String (Doc ann)
unparseCopsInfeasibility comment inf = do
    prettySystems <-
        case orientedCTrsToTrs (Inf.ctrs inf) of
            Nothing -> unparseCopsCTrs (Inf.ctrs inf)
            Just _ -> unparseCopsCTrsSystem (Inf.ctrs inf)
    pure
        $ vsep
            [ parens $ "PROBLEM" <+> "INFEASIBILITY"
            , parens $ "COMMENT" <+> pretty comment
            , prettySystems
            , prettyBlock "VAR" $ hsep $ map pretty $ nubOrd $ concatMap varsCondition (query inf)
            , parens $ "CONDITION" <+> concatWith (\l r -> l <> "," <+> r) [unparseCondition c | c <- query inf]
            ]
