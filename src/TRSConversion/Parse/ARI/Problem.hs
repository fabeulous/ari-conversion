{- |
Module      : Data.Conversion.Parse.ARI.Problem
Description : Parse a Problem in COPS format
-}
module TRSConversion.Parse.ARI.Problem (
  parseProblem,
)
where

import TRSConversion.Parse.ARI.CSCTrs (parseAriCSCTrs)
import TRSConversion.Parse.ARI.CSTrs (parseAriCSTrs)
import TRSConversion.Parse.ARI.CTrs (parseAriCTrs)
import TRSConversion.Parse.ARI.MSTrs (parseAriMsTrs)
import TRSConversion.Parse.ARI.MetaInfo (parseAriMetaInfo)
import TRSConversion.Parse.ARI.Trs (parseAriTrs)
import TRSConversion.Parse.ARI.Utils (ARIParser)
import TRSConversion.Problem.Problem (Problem (Problem))
import qualified TRSConversion.Problem.Problem as Prob
import Text.Megaparsec (choice, try)

parseProblem :: ARIParser Problem
parseProblem = do
  metaInfo <- parseAriMetaInfo
  system <-
    choice
      [ try $ Prob.Trs <$> parseAriTrs
      , try $ Prob.MSTrs <$> parseAriMsTrs
      , try $ Prob.CTrs <$> parseAriCTrs
      , try $ Prob.CSTrs <$> parseAriCSTrs
      , Prob.CSCTrs <$> parseAriCSCTrs
      ]
  pure $
    Problem
      { Prob.metaInfo = metaInfo
      , Prob.system = system
      }
