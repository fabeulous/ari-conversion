{- |
Module      : TRSConversion.Unparse.ARI.Problem
Description : Type definition Problems
-}
module TRSConversion.Unparse.ARI.Problem (
    unparseAriProblem,
)
where

import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, hardline, vsep, Pretty)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..))
import TRSConversion.Problem.Problem (Problem (..), System (..))
import TRSConversion.Unparse.ARI.CSCTrs (unparseAriCSCTrs)
import TRSConversion.Unparse.ARI.CSTrs (unparseAriCSTrs)
import TRSConversion.Unparse.ARI.CTrs (unparseAriCTrs)
import TRSConversion.Unparse.ARI.Infeasibility (unparseAriInfeasibility)
import TRSConversion.Unparse.ARI.Problem.MetaInfo (unparseAriMetaInfo)
import TRSConversion.Unparse.ARI.UnparseMsTrs (unparseAriMsTrs)
import TRSConversion.Unparse.ARI.UnparseTrs (unparseAriTrs)
import TRSConversion.Unparse.Utils (filterEmptyDocs)

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
