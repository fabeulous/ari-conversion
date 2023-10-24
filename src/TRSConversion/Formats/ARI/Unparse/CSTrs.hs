{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Formats.ARI.Unparse.CSTrs
Description : Unparser for CSTRSs
-}
module TRSConversion.Formats.ARI.Unparse.CSTrs (
  -- * ARI
  unparseAriCSTrs,
  -- ** Helpers
  unparseAriReplacementSig,
)
where

import qualified Data.Map as M
import Prettyprinter (Doc, Pretty, hsep, parens, pretty, vsep, (<+>))

import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))
import TRSConversion.Formats.ARI.Unparse.Problem.Rule (unparseAriSystems)
import TRSConversion.Unparse.Utils (filterEmptyDocs)

unparseAriCSTrs :: (Pretty f, Pretty v, Ord f) => CSTrs f v -> Either String (Doc ann)
unparseAriCSTrs cstrs = do
  ariSig <- unparseAriReplacementSig (signature cstrs) (replacementMap cstrs)
  let trsElements =
        [ formatString
        , ariSig
        , unparseAriSystems (rules cstrs)
        ]
  return $ vsep (filterEmptyDocs trsElements)
 where
  n = numSystems cstrs
  formatString = parens $ "format" <+> "CSTRS" <>
    if n > 1 then mempty <+> ":number" <+> pretty n else mempty

unparseAriReplacementSig :: (Ord f, Pretty f) => TrsSig f -> ReplacementMap f -> Either String (Doc ann)
unparseAriReplacementSig (FunSig fs) repMap = Right (vsep $ map prettySigLine fs)
 where
  repMapM = M.fromList repMap
  prettyM = M.map (\ints -> mempty <+> ":replacement-map" <+> (parens . hsep $ map pretty ints)) repMapM

  prettySigLine (Sig f a) = parens $ "fun" <+> pretty f <+> pretty a <> M.findWithDefault mempty f prettyM
