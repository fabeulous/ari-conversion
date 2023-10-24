{- |
Module      : TRSConversion.Formats.ARI.Unparse.Problem
Description : Type definition Problems
-}
module TRSConversion.Formats.ARI.Unparse.Problem (
    unparseAriProblem,
)
where

import Prettyprinter (Doc, vsep, Pretty)

import TRSConversion.Problem.Problem (Problem (..), System (..))
import TRSConversion.Formats.ARI.Unparse.CSCTrs (unparseAriCSCTrs)
import TRSConversion.Formats.ARI.Unparse.CSTrs (unparseAriCSTrs)
import TRSConversion.Formats.ARI.Unparse.CTrs (unparseAriCTrs)
import TRSConversion.Formats.ARI.Unparse.Infeasibility (unparseAriInfeasibility)
import TRSConversion.Formats.ARI.Unparse.Problem.MetaInfo (unparseAriMetaInfo)
import TRSConversion.Formats.ARI.Unparse.UnparseMsTrs (unparseAriMsTrs)
import TRSConversion.Formats.ARI.Unparse.UnparseTrs (unparseAriTrs)
import TRSConversion.Unparse.Utils (filterEmptyDocs)

unparseAriProblem :: (Ord f, Pretty f, Pretty v, Eq s, Pretty s) =>
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
