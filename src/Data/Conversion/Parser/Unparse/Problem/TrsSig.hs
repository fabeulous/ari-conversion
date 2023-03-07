-- |
-- Module      : Data.Conversion.Parser.Unparse.Problem.TrsSig
-- Description : Unparser for TrsSig
--
-- This module defines functions to unparse a 'TrsSig' into COPS and ARI format.
module Data.Conversion.Parser.Unparse.Problem.TrsSig
  ( unparseCopsTrsSig,
  )
where

import Data.Conversion.Parser.Unparse.Utils (prettyBlock)
import Data.Conversion.Problem.Common.Rule (Rule, ruleVars)
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Prettyprinter (Doc, Pretty, hsep, pretty, vsep)

-- | Pretty print a 'TrsSig' in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- * If the 'TrsSig' only has variables specified (via 'Vars'), then this is translated into
--      @(VAR x1 x2 ... xm)@ for each variable @xi@
--
-- * If the 'TrsSig' has variables and function symbols specified (via 'FullSig'), then this is translated into
--      @(VAR x1 ... xm)\n(SIG (f1 a1) ... (fn ai))@ for each variable @xi@ and function symbol @fi@ with arity @ai@
--
-- * If the 'TrsSig' has only function symbols specified (via 'FunSig'), then all variables in the TRS rules are extracted and this case
--      is treated like the 'FullSig' case. This is the reason for rules being given as an argument.
unparseCopsTrsSig :: (Eq v, Pretty f, Pretty v) => TrsSig f v -> [Rule f v] -> Doc ann
unparseCopsTrsSig trsSig rs = case trsSig of
  Vars vs -> prettyVars vs
  FullSig vs fs -> vsep [prettyVars vs, prettyCopsSig fs]
  FunSig fs -> unparseCopsTrsSig (FullSig (ruleVars rs) fs) rs -- Extract variables from TRS rules
  where
    prettyVars :: Pretty v => [v] -> Doc ann
    prettyVars vs = prettyBlock "VAR" (hsep $ map pretty vs)
    prettyCopsSig :: Pretty f => [Sig f] -> Doc ann
    prettyCopsSig fs = prettyBlock "SIG" (hsep $ map pretty fs)