{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Unparse.CSTrs
Description : Unparser for CSTRSs
-}
module TRSConversion.Unparse.CSTrs (
  -- * COPS
  unparseCopsCSTrs,
  -- ** Helpers
  copsReplacementMap,

  -- * ARI
  unparseAriCSTrs,
  -- ** Helpers
  unparseAriReplacementSig
)
where

import Data.List (group, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, Pretty, comma, hardline, hsep, nest, parens, pretty, punctuate, space, vsep, (<+>))
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Common.Rule (inferSigFromRules)
import qualified TRSConversion.Problem.Common.Rule as R
import TRSConversion.Problem.Common.Term (vars)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))
import TRSConversion.Unparse.Problem.Rule (unparseAriRules, unparseCopsRule)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)

{- | Unparse a CSTRS from the Haskell 'CSTrs' representation into
[COPS CSTRS format](http://project-coco.uibk.ac.at/problems/cstrs.php).
-}
unparseCopsCSTrs :: (Ord v, Pretty f, Pretty v) => CSTrs f v -> Either String (Doc ann)
unparseCopsCSTrs cstrs = do
  pure $
    vsep
      [ prettyBlock "VAR" (hsep [pretty v | v <- vs])
      , prettyBlock "REPLACEMENT-MAP" (nest 2 (hardline <> copsReplacementMap (replacementMap cstrs)) <> hardline)
      , prettyBlock "RULES" (nest 2 (vsep $ mempty : [unparseCopsRule r | r <- rules cstrs]) <> hardline)
      ]
 where
  vs = varsOfTrs (signature cstrs) (rules cstrs)

  varsOfTrs (Vars vas) _ = vas
  varsOfTrs _ rs = map head . group . sort $ concatMap varsOfRules rs

  varsOfRules r = vars (R.lhs r) ++ vars (R.rhs r)

copsReplacementMap :: Pretty f => ReplacementMap f -> Doc ann
copsReplacementMap repMap =
  vsep
    [ parens $
      pretty f
        <> if null poss
          then mempty
          else space <> hsep (punctuate comma (map pretty poss))
    | (f, poss) <- repMap
    ]

unparseAriCSTrs :: (Pretty f, Pretty v, Ord f) => CSTrs f v -> Either String (Doc ann)
unparseAriCSTrs cstrs = do
  ariSig <- unparseAriReplacementSig (rules cstrs) (signature cstrs) (replacementMap cstrs)
  let trsElements =
        [ parens "format CSTRS"
        , ariSig
        , fromMaybe mempty $ unparseAriRules (rules cstrs)
        ]
  return $ vsep (filterEmptyDocs trsElements)

unparseAriReplacementSig :: (Ord f, Pretty f) => [R.Rule f v] -> TrsSig f v -> ReplacementMap f -> Either String (Doc ann)
unparseAriReplacementSig rs sig repMap = go sig
 where
  repMapM = M.fromList repMap
  prettyM = M.map (\ints -> mempty <+> ":replacement-map" <+> (parens . hsep $ map pretty ints)) repMapM

  go (FunSig fs) = Right (vsep $ map prettySigLine fs)
  go (FullSig _ fs) = Right (vsep $ map prettySigLine fs)
  go (Vars _) = go . FunSig =<< inferSigFromRules rs

  prettySigLine (Sig f a) = parens $ "fun" <+> pretty f <+> pretty a <> M.findWithDefault mempty f prettyM
