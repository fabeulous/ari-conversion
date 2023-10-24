{- |
Module      : TRSConversion.Unparse.COPS.Problem
Description : Type definition Problems
-}
module TRSConversion.Unparse.COPS.Problem (
    unparseCopsProblem,
    unparseCopsCOMProblem,
)
where

import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, hardline, vsep, Pretty)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..))
import TRSConversion.Problem.Problem (Problem (..), System (..))
import TRSConversion.Unparse.COPS.COM (unparseCopsCOM)
import TRSConversion.Unparse.COPS.CSCTrs (unparseCopsCSCTrs)
import TRSConversion.Unparse.COPS.CSTrs (unparseCopsCSTrs)
import TRSConversion.Unparse.COPS.CTrs (unparseCopsCTrs)
import TRSConversion.Unparse.COPS.Infeasibility (unparseCopsInfeasibility)
import TRSConversion.Unparse.COPS.Problem.MetaInfo (unparseCopsMetaInfo)
import TRSConversion.Unparse.COPS.UnparseMsTrs (unparseCopsMsTrs)
import TRSConversion.Unparse.COPS.UnparseTrs (unparseCopsTrs)
import TRSConversion.Unparse.Utils (filterEmptyDocs)

unparseCopsProblem :: (Pretty f, Pretty v, Pretty s, Ord f, Ord v) =>
  Problem f v s -> Either String (Doc ann)
unparseCopsProblem problem = do
    prettySystem <- prettySystemErr
    pure (prettySystem <> hardline <> prettyMeta)
  where
    prettySystemErr =
        case system problem of
            Trs trs -> unparseCopsTrs trs
            MSTrs trs -> unparseCopsMsTrs trs
            CTrs trs -> unparseCopsCTrs trs
            CSTrs trs -> unparseCopsCSTrs trs
            CSCTrs trs -> unparseCopsCSCTrs trs
            Infeasibility inf -> unparseCopsInfeasibility originComment inf

    prettyMeta = unparseCopsMetaInfo metaInfo'
    metaInfo' = case system problem of
        Infeasibility _ -> (metaInfo problem){origin = Nothing}
        _ -> metaInfo problem
    originComment = fromMaybe "" (origin (metaInfo problem))

unparseCopsCOMProblem :: (Ord f, Ord v, Pretty f, Pretty v) =>
  Problem f v s -> Either String (Doc ann)
unparseCopsCOMProblem problem =
    case system problem of
        Trs trs -> do
            prettySystems <- unparseCopsCOM originComment trs
            pure $ vsep [prettySystems, unparseCopsMetaInfo metaInfo']
        _ -> Left "COPS only supports TRSs in commutation problems"
  where
    metaInfo' = (metaInfo problem){origin = Nothing}
    originComment = fromMaybe "" (origin (metaInfo problem))

