{- |
Module      : Data.Conversion.Formats.COPS.Parse.Problem
Description : Parser a Problem in COPS format
-}
module TRSConversion.Formats.COPS.Parse.Problem (
  parseProblem,
  parseCOMProblem,
)
where

import Control.Applicative ((<|>))
import TRSConversion.Formats.COPS.Parse.COM (parseCopsCom)
import TRSConversion.Formats.COPS.Parse.CSCTrs (parseCopsCSCTrs)
import TRSConversion.Formats.COPS.Parse.CSTrs (parseCopsCSTrs)
import TRSConversion.Formats.COPS.Parse.CTrs (parseCopsCTrs)
import TRSConversion.Formats.COPS.Parse.Infeasibility (parseCopsInfeasibility)
import TRSConversion.Formats.COPS.Parse.MSTrs (parseCopsMsTrs)
import TRSConversion.Formats.COPS.Parse.MetaInfo (parseCopsMetaInfoBlock)
import TRSConversion.Formats.COPS.Parse.Trs (parseCopsTrs)
import TRSConversion.Formats.COPS.Parse.Utils (COPSParser)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import TRSConversion.Problem.Problem (Problem (Problem))
import qualified TRSConversion.Problem.Problem as Prob
import Text.Megaparsec (choice, option, try)

parseProblem :: COPSParser (Problem String String String)
parseProblem =
  parseCOMProblem <|> parseINFProblem <|> do
    system <-
      choice
        [ try $ Prob.Trs <$> parseCopsTrs
        , try $ Prob.MSTrs <$> parseCopsMsTrs
        , try $ Prob.CTrs <$> parseCopsCTrs
        , try $ Prob.CSTrs <$> parseCopsCSTrs
        , Prob.CSCTrs <$> parseCopsCSCTrs
        ]
    metaInfo <- option emptyMetaInfo parseCopsMetaInfoBlock
    pure $
      Problem
        { Prob.metaInfo = metaInfo
        , Prob.system = system
        }

parseCOMProblem :: COPSParser (Problem String String String)
parseCOMProblem = do
  (copsComment, trs) <- parseCopsCom
  metaInfo <- option emptyMetaInfo parseCopsMetaInfoBlock
  pure $
    Problem
      { Prob.metaInfo = metaInfo{origin = copsComment}
      , Prob.system = Prob.Trs trs
      }

parseINFProblem :: COPSParser (Problem String String String)
parseINFProblem = do
  (copsComment, inf) <- parseCopsInfeasibility
  metaInfo <- option emptyMetaInfo parseCopsMetaInfoBlock
  pure $
    Problem
      { Prob.metaInfo = metaInfo{origin = copsComment}
      , Prob.system = Prob.Infeasibility inf
      }
