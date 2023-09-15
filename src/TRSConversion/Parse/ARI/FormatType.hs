{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TRSConversion.Parse.ARI.FormatType (
    parseFormatType,
)
where

import Control.Monad (unless)
import Data.Text (Text, pack)
import Text.Megaparsec (ParseError, Stream, Tokens, getOffset, option, parseError, try, (<|>))
import qualified Text.Megaparsec.Error.Builder as E

import TRSConversion.Parse.ARI.CTrs (pCondType)
import TRSConversion.Parse.ARI.Utils (
    ARIParser,
    ident,
    keyword,
    naturalNumber,
    nonPositiveNumberError,
    sExpr,
 )
import TRSConversion.Problem.Problem (FormatType (..))

parseFormatType :: ARIParser (Int, FormatType)
parseFormatType =
    sExpr "format" $ do
        o <- getOffset
        name <- ident
        ftype <- case name of
            "TRS" -> trsFormat
            "MSTRS" -> msTrsFormat
            "LCTRS" -> lcTrsFormat
            "CTRS" -> try infeasibilityFormat <|> cTrsFormat
            "CSTRS" -> csTrsFormat
            "CSCTRS" -> cscTrsFormat
            _ -> parseError $ unknownFormatType (pack name) o
        pure (o, ftype)

unknownFormatType :: (Stream s, Tokens s ~ Text) => Text -> Int -> ParseError s e
unknownFormatType name offSet =
    E.err offSet $ E.utoks name

lcTrsFormat :: ARIParser FormatType
lcTrsFormat = LCTrsFormat <$> optNumber

cscTrsFormat :: ARIParser FormatType
cscTrsFormat = CSCTrsFormat <$> pCondType <*> optNumber

csTrsFormat :: ARIParser FormatType
csTrsFormat = CSTrsFormat <$> optNumber

cTrsFormat :: ARIParser FormatType
cTrsFormat = CTrsFormat <$> pCondType <*> optNumber

infeasibilityFormat :: ARIParser FormatType
infeasibilityFormat = InfeasibilityFormat <$> pCondType <* keyword ":problem" <* keyword "infeasibility"

msTrsFormat :: ARIParser FormatType
msTrsFormat = MSTrsFormat <$> optNumber

trsFormat :: ARIParser FormatType
trsFormat = TrsFormat <$> optNumber

optNumber :: ARIParser Int
optNumber =
    option 1 $ do
        _ <- keyword ":number"
        o <- getOffset
        n <- naturalNumber
        unless (n > 0) $ parseError (nonPositiveNumberError n o)
        pure n
