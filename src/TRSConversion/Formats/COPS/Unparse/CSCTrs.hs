{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Formats.COPS.Unparse.CSCTrs
Description : Unparser for CSCTRSs
-}
module TRSConversion.Formats.COPS.Unparse.CSCTrs (
  -- * COPS
  unparseCopsCSCTrs,
)
where

import qualified Data.IntMap as IntMap
import Prettyprinter (Doc, Pretty, hardline, hsep, nest, pretty, vsep)

import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), vars)
import TRSConversion.Formats.COPS.Unparse.CSTrs (copsReplacementMap)
import TRSConversion.Formats.COPS.Unparse.CTrs (prettyCRule, prettyCondType)
import TRSConversion.Unparse.Utils (prettyBlock)

{- | Unparse a first-order TRS from the Haskell 'Trs' representation into
[COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).

Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
unparse each part of the 'Trs'.

See the tests for examples of expected output.
-}
unparseCopsCSCTrs :: (Ord v, Pretty f, Pretty v) => CSCTrs f v -> Either String (Doc ann)
unparseCopsCSCTrs CSCTrs{ctrs = system, replacementMap = repMap}
  | numSystems system /= 1 = error "COPS format doesn't support CSCTRSs with multiple systems"
  | otherwise = do
      pure $
        vsep
          [ prettyBlock "CONDITIONTYPE" (prettyCondType $ conditionType system)
          , prettyBlock "VAR" (hsep [pretty v | v <- vs])
          , prettyBlock "REPLACEMENT-MAP" (nest 2 (hardline <> copsReplacementMap repMap) <> hardline)
          , prettyBlock "RULES" (nest 2 (vsep $ mempty : [prettyCRule r | r <- rs]) <> hardline)
          ]
 where
  rs = rules system IntMap.! 1
  vs = vars rs
