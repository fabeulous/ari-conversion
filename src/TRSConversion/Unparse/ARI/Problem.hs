{- |
Module      : TRSConversion.Unparse.Problem
Description : Type definition Problems
-}
module TRSConversion.Unparse.Problem (
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
