{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Formats.ARI.Parse.Infeasibility
( parseAriInfeasibility,
  parseAriTRSInfeasibility',
  parseAriCTRSInfeasibility',
  parseInfQuery,
  )
where

import Control.Applicative (many)
import Text.Megaparsec (try, (<|>))

import TRSConversion.Formats.ARI.Parse.CTrs (pCondType, parseAriCTrs', parseCondition)
import TRSConversion.Formats.ARI.Parse.Trs (parseAriTrs')
import TRSConversion.Formats.ARI.Parse.Utils (ARIParser, FunSymb, VarSymb, keyword, sExpr)
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), CondType, Condition, trsToOrientedCTrs)
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import TRSConversion.Problem.Trs.Sig (Sig)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (FunSig))

parseAriInfeasibility :: ARIParser (Infeasibility FunSymb VarSymb)
parseAriInfeasibility = ctrsInf <|> trsInf
 where
   ctrsInf = do
      condType <- try $ sExpr "format" (keyword "CTRS" *> pCondType <* keyword ":problem" <* keyword "infeasibility")
      parseAriCTRSInfeasibility' condType
   trsInf = do
      _ <- try $ sExpr "format" (keyword "TRS" <* keyword ":problem" <* keyword "infeasibility")
      parseAriTRSInfeasibility'


parseAriCTRSInfeasibility' :: CondType -> ARIParser (Infeasibility FunSymb VarSymb)
parseAriCTRSInfeasibility' condType = do
    sys <- parseAriCTrs' condType 1
    let FunSig funSig = signature sys
    q <- parseInfQuery funSig
    pure $ Infeasibility{ctrs = sys, query = q, isTrs = False}

parseAriTRSInfeasibility' :: ARIParser (Infeasibility FunSymb VarSymb)
parseAriTRSInfeasibility' = do
    trsSys <- parseAriTrs' 1
    let ctrsSys = trsToOrientedCTrs trsSys
    let FunSig funSig = signature ctrsSys
    q <- parseInfQuery funSig
    pure $ Infeasibility{ctrs = ctrsSys, query = q, isTrs = True}

parseInfQuery :: [Sig FunSymb] -> ARIParser [Condition FunSymb VarSymb]
parseInfQuery funSig =
    sExpr "infeasible?" $ many (parseCondition funSig)
