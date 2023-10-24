{- |
Module      : TRSConversion.Formats.COPS.Unparse.Problem
Description : Type definition Problems
-}
module TRSConversion.Formats.COPS.Unparse.Problem (
    unparseCopsProblem,
    unparseCopsCOMProblem,
)
where

import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, Pretty, hardline, vsep)

import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..))
import TRSConversion.Problem.Problem (Problem (..), System (..))
import TRSConversion.Formats.COPS.Unparse.COM (unparseCopsCOM)
import TRSConversion.Formats.COPS.Unparse.CSCTrs (unparseCopsCSCTrs)
import TRSConversion.Formats.COPS.Unparse.CSTrs (unparseCopsCSTrs)
import TRSConversion.Formats.COPS.Unparse.CTrs (unparseCopsCTrs)
import TRSConversion.Formats.COPS.Unparse.Infeasibility (unparseCopsInfeasibility)
import TRSConversion.Formats.COPS.Unparse.Problem.MetaInfo (unparseCopsMetaInfo)
import TRSConversion.Formats.COPS.Unparse.UnparseMsTrs (unparseCopsMsTrs)
import TRSConversion.Formats.COPS.Unparse.UnparseTrs (unparseCopsTrs)

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

