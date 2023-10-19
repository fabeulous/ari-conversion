{- |
Module      : TRSConversion.Unparse.Problem
Description : Type definition Problems
-}
module TRSConversion.Unparse.Problem (
    unparseCopsProblem,
    unparseCopsCOMProblem,
    unparseAriProblem,
)
where

import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, hardline, vsep, Pretty)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..))
import TRSConversion.Problem.Problem (Problem (..), System (..))
import TRSConversion.Unparse.COM (unparseCopsCOM)
import TRSConversion.Unparse.CSCTrs (unparseAriCSCTrs, unparseCopsCSCTrs)
import TRSConversion.Unparse.CSTrs (unparseAriCSTrs, unparseCopsCSTrs)
import TRSConversion.Unparse.CTrs (unparseAriCTrs, unparseCopsCTrs)
import TRSConversion.Unparse.Infeasibility (unparseAriInfeasibility, unparseCopsInfeasibility)
import TRSConversion.Unparse.Problem.MetaInfo (unparseAriMetaInfo, unparseCopsMetaInfo)
import TRSConversion.Unparse.UnparseMsTrs (unparseAriMsTrs, unparseCopsMsTrs)
import TRSConversion.Unparse.UnparseTrs (unparseAriTrs, unparseCopsTrs)
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

unparseAriProblem :: (Ord f, Eq v, Pretty f, Pretty v, Eq s, Pretty s) =>
  Problem f v s -> Either String (Doc ann)
unparseAriProblem problem = do
    prettySystem <- prettySystemErr
    pure (vsep $ filterEmptyDocs [prettyMeta, prettySystem])
  where
    prettySystemErr =
        case system problem of
            Trs trs -> unparseAriTrs trs
            MSTrs trs -> pure $ unparseAriMsTrs trs
            CTrs trs -> unparseAriCTrs trs
            CSTrs trs -> unparseAriCSTrs trs
            CSCTrs trs -> unparseAriCSCTrs trs
            Infeasibility inf -> unparseAriInfeasibility inf
    prettyMeta = unparseAriMetaInfo (metaInfo problem)
