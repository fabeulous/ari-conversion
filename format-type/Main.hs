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

import TRSConversion.Parse.ARI.FormatType (parseFormatType)
import TRSConversion.Parse.ARI.Utils (ARIParser)
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Parse.Utils (parseIO)
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
    formatType <- parseIO (ARI.toParser parseHead) filename fileContents
    putStrLn $ case formatType of
        TrsFormat _ -> "trs"
        MSTrsFormat _ -> "mstrs"
        CTrsFormat _ _ -> "ctrs"
        CSTrsFormat _ -> "cstrs"
        CSCTrsFormat _ _ -> "csctrs"
        LCTrsFormat _ -> "lctrs"

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
