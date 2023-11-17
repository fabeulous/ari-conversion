module Main (main) where

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Data.Text.IO as Text
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (takeWhileP)
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

import TRSConversion.Formats.ARI.Parse.FormatType (parseFormatType)
import TRSConversion.Formats.ARI.Parse.Utils (ARIParser)
import qualified TRSConversion.Formats.ARI.Parse.Utils as ARI
import TRSConversion.Parse.Utils (parseIOpartial)
import TRSConversion.Problem.Problem (FormatType (..))

usage :: String -> String
usage name = "Usage: " ++ name ++ " FILE"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> runApp filename
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ usage name
            exitFailure

runApp :: FilePath -> IO ()
runApp filename = do
    fileContents <- Text.readFile filename
    formatType <- parseIOpartial (ARI.toParser parseHead) filename fileContents
    putStrLn $ case formatType of
        TrsFormat n
            | n == 1 -> "trs"
            | n == 2 -> "2trs"
            | otherwise -> "multi-trs"
        MSTrsFormat n
            | n == 1 -> "mstrs"
            | n == 2 -> "2mstrs"
            | otherwise -> "multi-mstrs"
        CTrsFormat _ n
            | n == 1 -> "ctrs"
            | n == 2 -> "2ctrs"
            | otherwise -> "multi-ctrs"
        CSTrsFormat _ -> "cstrs"
        CSCTrsFormat _ n
            | n == 1 -> "csctrs"
            | n == 2 -> "2csctrs"
            | otherwise -> "multi-csctrs"
        LCTrsFormat n
            | n == 1 -> "lctrs"
            | n == 2 -> "2lctrs"
            | otherwise -> "multi-lctrs"
        InfeasibilityTrsFormat -> "infeasibility-trs"
        InfeasibilityCTrsFormat _ -> "infeasibility-ctrs"

parseHead :: ARIParser FormatType
parseHead = do
    skipSpaces -- skip whitespace and metainfo/comments
    (_, formatType) <- parseFormatType
    pure formatType

skipSpaces :: ARIParser ()
skipSpaces = L.space space1 lineComment empty

lineComment :: ARIParser ()
lineComment =
    void $ char ';' >> takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
