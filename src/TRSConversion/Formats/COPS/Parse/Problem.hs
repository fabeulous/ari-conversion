{- |
Module      : Data.Conversion.Parse.COPS.Problem
Description : Parser a Problem in COPS format
-}
module TRSConversion.Parse.COPS.Problem (
  parseProblem,
  parseCOMProblem,
)
where

import Control.Applicative ((<|>))
import TRSConversion.Parse.COPS.COM (parseCopsCom)
import TRSConversion.Parse.COPS.CSCTrs (parseCopsCSCTrs)
import TRSConversion.Parse.COPS.CSTrs (parseCopsCSTrs)
import TRSConversion.Parse.COPS.CTrs (parseCopsCTrs)
import TRSConversion.Parse.COPS.Infeasibility (parseCopsInfeasibility)
import TRSConversion.Parse.COPS.MSTrs (parseCopsMsTrs)
import TRSConversion.Parse.COPS.MetaInfo (parseCopsMetaInfoBlock)
import TRSConversion.Parse.COPS.Trs (parseCopsTrs)
import TRSConversion.Parse.COPS.Utils (COPSParser)
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
