{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Unparse.ARI.CSCTrs
Description : Unparser for CSCTRSs
-}
module TRSConversion.Unparse.ARI.CSCTrs (
  -- * ARI
  unparseAriCSCTrs,
)
where

import Data.List (group, sort)
import Prettyprinter (Doc, Pretty, hardline, hsep, nest, parens, pretty, vsep, (<+>))
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), inferSigFromRules)
import TRSConversion.Problem.Common.Term (vars)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..), Sig (..))
import TRSConversion.Unparse.ARI.CTrs (prettyAriConditionType, unparseAriCSystems)
import TRSConversion.Unparse.ARI.Utils (filterEmptyDocs)
import TRSConversion.Problem.CSTrs.CSTrs (ReplacementMap)
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IntMap

unparseAriCSCTrs :: (Pretty f, Pretty v, Ord f) => CSCTrs f v -> Either String (Doc ann)
unparseAriCSCTrs CSCTrs{ctrs = system, replacementMap = repMap} = do
  ariSig <- unparseAriCSCTrsSig (signature system) repMap
  let trsElements =
        [ parens $ "format CSCTRS" <+> prettyAriConditionType (conditionType system)
        , ariSig
        , unparseAriCSystems (rules system)
        ]
  return $ vsep (filterEmptyDocs trsElements)

unparseAriCSCTrsSig :: (Ord f, Pretty f) => TrsSig f -> ReplacementMap f -> Either String (Doc ann)
unparseAriCSCTrsSig (FunSig fs) repMap = Right (vsep $ map prettySigLine fs)
 where
  repMapM = M.fromList repMap
  prettyM = M.map (\ints -> mempty <+> ":replacement-map" <+> (parens . hsep $ map pretty ints)) repMapM

  prettySigLine (Sig f a) = parens $ "fun" <+> pretty f <+> pretty a <> M.findWithDefault mempty f prettyM
