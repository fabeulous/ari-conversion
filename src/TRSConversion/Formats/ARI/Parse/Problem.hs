{- |
Module      : Data.Conversion.Formats.ARI.Parse.Problem
Description : Parse a Problem in COPS format
-}
module TRSConversion.Formats.ARI.Parse.Problem (
  parseProblem,
  parseProblem',
)
where

import TRSConversion.Formats.ARI.Parse.CSCTrs (parseAriCSCTrs')
import TRSConversion.Formats.ARI.Parse.CSTrs (parseAriCSTrs')
import TRSConversion.Formats.ARI.Parse.CTrs (parseAriCTrs')
import TRSConversion.Formats.ARI.Parse.MSTrs (parseAriMsTrs')
import TRSConversion.Formats.ARI.Parse.MetaInfo (parseAriMetaInfo)
import TRSConversion.Formats.ARI.Parse.Trs (parseAriTrs')
import TRSConversion.Formats.ARI.Parse.Utils (ARIParser, spaces, FunSymb, VarSymb, SortSymb, sExpr'')
import TRSConversion.Problem.Problem (Problem (Problem), FormatType (..), mapSystem)
import qualified TRSConversion.Problem.Problem as Prob
import TRSConversion.Formats.ARI.Parse.FormatType (parseFormatType)
import qualified Text.Megaparsec.Error.Builder as E
import Text.Megaparsec (MonadParsec(parseError))
import TRSConversion.Formats.ARI.Parse.Infeasibility (parseAriTRSInfeasibility', parseAriCTRSInfeasibility')
import TRSConversion.Parse.Utils (unToken)
import TRSConversion.Problem.Trs.Trs (emptyTrs)
import Text.Megaparsec (many)

parseProblem :: ARIParser (Problem String String String)
parseProblem = do
  prob@(Problem {Prob.system = system}) <- parseProblem'
  pure $ prob { Prob.system = mapSystem unToken unToken unToken system }

parseProblem' :: ARIParser (Problem FunSymb VarSymb SortSymb)
parseProblem' = do
  metaInfo <- parseAriMetaInfo
  spaces
  (o, ft) <- parseFormatType
  system <- case ft of
    TrsFormat n -> Prob.Trs <$> parseAriTrs' n
    MSTrsFormat n -> Prob.MSTrs <$> parseAriMsTrs' n
    CTrsFormat condType n -> Prob.CTrs <$> parseAriCTrs' condType n
    CSTrsFormat n -> Prob.CSTrs <$> parseAriCSTrs' n
    CSCTrsFormat condType n -> Prob.CSCTrs <$> parseAriCSCTrs' condType n
    InfeasibilityCTrsFormat condType -> Prob.Infeasibility <$> parseAriCTRSInfeasibility' condType
    InfeasibilityTrsFormat -> Prob.Infeasibility <$> parseAriTRSInfeasibility'
    LCTrsFormat _ -> Prob.Trs <$> (many sExpr'' *> return emptyTrs)
  pure $
    Problem
      { Prob.metaInfo = metaInfo
      , Prob.system = system
      }
