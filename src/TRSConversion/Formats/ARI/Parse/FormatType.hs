{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TRSConversion.Formats.ARI.Parse.FormatType (
    parseFormatType,
)
where

import Control.Monad (unless)
import Data.Text (Text)
import Text.Megaparsec (ParseError, Stream, Tokens, getOffset, option, optional, parseError, try, (<|>))
import qualified Text.Megaparsec.Error.Builder as E

import TRSConversion.Formats.ARI.Parse.CTrs (pCondType)
import TRSConversion.Formats.ARI.Parse.Utils (
    ARIParser,
    ident,
    keyword,
    naturalNumber,
    nonPositiveNumberError,
    sExpr,
 )
import TRSConversion.Parse.Utils (tokenText, tokenValue)
import TRSConversion.Problem.Problem (FormatType (..))

parseFormatType :: ARIParser (Int, FormatType)
parseFormatType =
    sExpr "format" $ do
        o <- getOffset
        name <- ident
        ftype <- case tokenValue name of
            "TRS" -> try infeasibilityTrsFormat <|> trsFormat
            "MSTRS" -> msTrsFormat
            "LCTRS" -> lcTrsFormat
            "CTRS" -> try infeasibilityCTrsFormat <|> cTrsFormat
            "CSTRS" -> csTrsFormat
            "CSCTRS" -> cscTrsFormat
            _ -> parseError $ unknownFormatType (tokenText name) o
        pure (o, ftype)

unknownFormatType :: (Stream s, Tokens s ~ Text) => Text -> Int -> ParseError s e
unknownFormatType name offSet =
    E.err offSet $ E.utoks name

lcTrsFormat :: ARIParser FormatType
lcTrsFormat = do
    n <- optNumber
    _ <- optional $ keyword ":standard" *> ident
    pure $ LCTrsFormat n

cscTrsFormat :: ARIParser FormatType
cscTrsFormat = CSCTrsFormat <$> pCondType <*> optNumber

csTrsFormat :: ARIParser FormatType
csTrsFormat = CSTrsFormat <$> optNumber

cTrsFormat :: ARIParser FormatType
cTrsFormat = CTrsFormat <$> pCondType <*> optNumber

infeasibilityCTrsFormat :: ARIParser FormatType
infeasibilityCTrsFormat = InfeasibilityCTrsFormat <$> pCondType <* keyword ":problem" <* keyword "infeasibility"

infeasibilityTrsFormat :: ARIParser FormatType
infeasibilityTrsFormat = pure InfeasibilityTrsFormat <* keyword ":problem" <* keyword "infeasibility"

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
