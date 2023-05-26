{- |
Module      : Data.Conversion.Parse.COPS.Problem
Description : Parser a Problem in COPS format
-}
module TRSConversion.Parse.COPS.Problem (
  parseProblem,
)
where

import TRSConversion.Parse.COPS.CSCTrs (parseCopsCSCTrs)
import TRSConversion.Parse.COPS.CSTrs (parseCopsCSTrs)
import TRSConversion.Parse.COPS.CTrs (parseCopsCTrs)
import TRSConversion.Parse.COPS.MSTrs (parseCopsMsTrs)
import TRSConversion.Parse.COPS.MetaInfo (parseCopsMetaInfoBlock)
import TRSConversion.Parse.COPS.Trs (parseCopsTrs)
import TRSConversion.Parse.COPS.Utils (COPSParser)
import TRSConversion.Problem.Common.MetaInfo (emptyMetaInfo)
import TRSConversion.Problem.Problem (Problem (Problem))
import qualified TRSConversion.Problem.Problem as Prob
import Text.Megaparsec (choice, option, try)

parseProblem :: COPSParser Problem
parseProblem = do
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
