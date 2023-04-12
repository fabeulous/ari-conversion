{- |
Module      : TRSConversion.Unparse.Problem
Description : Type definition Problems
-}
module TRSConversion.Unparse.Problem (
    unparseCopsProblem,
    unparseAriProblem,
)
where

import Prettyprinter (Doc, hardline)
import TRSConversion.Problem.Problem (Problem (..), System (..))
import TRSConversion.Unparse.Problem.MetaInfo (unparseAriMetaInfo, unparseCopsMetaInfo)
import TRSConversion.Unparse.UnparseMsTrs (unparseAriMsTrs, unparseCopsMsTrs)
import TRSConversion.Unparse.UnparseTrs (unparseAriTrs, unparseCopsTrs)

unparseCopsProblem :: Problem -> Either String (Doc ann)
unparseCopsProblem problem = do
    prettySystem <- prettySystemErr
    pure (prettySystem <> hardline <> prettyMeta)
  where
    prettySystemErr =
        case system problem of
            Trs trs -> unparseCopsTrs trs
            MSTrs trs -> unparseCopsMsTrs trs
    prettyMeta = unparseCopsMetaInfo (metaInfo problem)

unparseAriProblem :: Problem -> Either String (Doc ann)
unparseAriProblem problem = do
    prettySystem <- prettySystemErr
    pure (prettyMeta <> hardline <> prettySystem)
  where
    prettySystemErr =
        case system problem of
            Trs trs -> unparseAriTrs trs
            MSTrs trs -> pure $ unparseAriMsTrs trs
    prettyMeta = unparseAriMetaInfo (metaInfo problem)
