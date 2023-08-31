module Main where

import Control.Applicative (empty)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (parse, takeWhileP)
import Text.Megaparsec.Char (char, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ShowErrorComponent, errorBundlePretty)

import TRSConversion.Parse.ARI.FormatType (parseFormatType)
import TRSConversion.Parse.ARI.Utils (ARIParser)
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Parse.Utils (Parser)
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
lineComment = do
    _ <- char ';'
    _ <- takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
    pure ()

parseIO :: ShowErrorComponent e => Parser e a -> String -> Text -> IO a
parseIO p inpName inp =
    case parse p inpName inp of
        Left err -> do
            putStrLn "Error: invalid input"
            putStr $ errorBundlePretty err
            exitFailure
        Right trs -> return trs
