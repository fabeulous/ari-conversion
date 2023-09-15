module Main (main) where

import qualified Data.Text.IO as Text
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified TRSConversion.Parse.ARI.Problem as ARI
import qualified TRSConversion.Parse.ARI.Utils as ARI
import TRSConversion.Parse.Utils (parseIO)
import qualified TRSConversion.Problem.Problem as Prob

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> runApp filename
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ usage name
            exitFailure

usage :: String -> String
usage name = "Usage: " ++ name ++ " FILE"

runApp :: FilePath -> IO ()
runApp fp = do
    fileContent <- Text.readFile fp
    problem <- parseIO (ARI.toParser ARI.parseProblem) fp fileContent
    putStr $ case Prob.system problem of
        (Prob.Trs _) -> "trs"
        (Prob.MSTrs _) -> "mstrs"
        (Prob.CTrs _) -> "ctrs"
        (Prob.CSTrs _) -> "cstrs"
        (Prob.CSCTrs _) -> "csctrs"
        (Prob.Infeasibility _) -> "infeasibility"
    putStrLn " successfully parsed"

    exitSuccess
