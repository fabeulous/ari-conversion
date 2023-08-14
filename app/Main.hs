module Main (main) where

import Control.Monad (unless)
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Prettyprinter (Doc)
import System.Console.GetOpt (
  ArgDescr (NoArg, ReqArg),
  ArgOrder (Permute),
  OptDescr (..),
  getOpt,
  usageInfo,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (Handle, IOMode (WriteMode), hClose, hPrint, hPutStrLn, openFile, stderr, stdout)
import Text.Megaparsec (eof, errorBundlePretty, parse)

import qualified TRSConversion.Parse.ARI.Problem as ARI
import qualified TRSConversion.Parse.ARI.Utils as ARI
import qualified TRSConversion.Parse.COPS.Problem as COPS
import qualified TRSConversion.Parse.COPS.Utils as COPS
import TRSConversion.Parse.Utils (Parser)
import TRSConversion.Unparse.Problem (unparseAriProblem, unparseCopsProblem, unparseCopsCOMProblem)

data Format
  = COPS
  | ARI
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | @Config@ holds the information parsed the options given on the comand line.
data Config = Config
  { confTarget :: Maybe String
  , confSource :: Maybe String
  , confCommutationFlag :: Bool
  , confOutputHandle :: Handle
  }

defaultConfig :: Config
defaultConfig =
  Config
    { confSource = Nothing
    , confTarget = Nothing
    , confCommutationFlag = False
    , confOutputHandle = stdout
    }

options :: [OptDescr (Config -> IO Config)]
options =
  [ Option
      ['f']
      ["from"]
      (ReqArg (\s c -> pure c{confSource = Just s}) "FORMAT")
      "source format"
  , Option
      ['t']
      ["to"]
      (ReqArg (\s c -> pure c{confTarget = Just s}) "FORMAT")
      "target format"
  , Option
      ['o']
      ["output"]
      ( ReqArg
          ( \s c -> do
              fileHandle <- openFile s WriteMode
              pure $ c{confOutputHandle = fileHandle}
          )
          "FILE"
      )
      "write output to FILE (defaults to stdout)"
  , Option
      ['h']
      ["help"]
      ( NoArg
          ( \_ -> usage stdout >> exitSuccess
          )
      )
      "print this message"
  , Option
      []
      ["commutation", "comm"]
      ( NoArg (\c -> pure $ c{confCommutationFlag = True})
      )
      "print the problem as a COMMUTATION problem in COPS"
  ]

usage :: Handle -> IO ()
usage handle = do
  execName <- getProgName
  hPutStrLn handle (usageInfo (header execName) options)
 where
  header execName =
    unlines
      [ "Usage: " ++ execName ++ " -f FORMAT -t FORMAT [OPTIONS] FILE"
      , "Convert problems involving term-rewrite systems between formats."
      , ""
      , "It is mandatory to give a source format (-f) and target format (-t)."
      , "Moreover the input FILE must contain a problem in the source format."
      , "The following FORMATs are supported: COPS, ARI. The problem type is"
      , "inferred from the input and may be: TRS, MSTRS, CTRS, CSTRS, CSCTRS."
      ]

{- | @trs-conversion-exe@ entry point. Can be run by calling
> stack build
> stack exec trs-conversion-exe

Currently just contains some simple hard-coded examples to illustrate
parsing and unparsing functionality.
-}
main :: IO ()
main = do
  args <- getArgs
  let (opts, nonOpts, errs) = getOpt Permute options args
  unless (null errs) $ do
    hPutStrLn stderr $ "Error(s) parsing arguments:\n" ++ show errs
    usage stderr
    exitFailure
  conf <- foldl (>>=) (pure defaultConfig) opts
  case nonOpts of
    [inputFile] ->
      case contextFromConfig conf of
        Left err -> do
          hPutStrLn stderr err
          usage stderr
          exitFailure
        Right ctxt -> runApp ctxt inputFile
    _ -> do
      hPutStrLn stderr "Error: expected exactly one input file"
      usage stderr
      exitFailure

-- | The @Context@ holds the the configuration after validation.
data Context = Context
  { target :: Format
  , source :: Format
  , commutationFlag :: Bool
  , outputHandle :: Handle
  }

contextFromConfig :: Config -> Either String Context
contextFromConfig conf = do
  srcName <- maybe (Left "Error: missing source format (-f)") Right $ confSource conf
  trgName <- maybe (Left "Error: missing target format (-t)") Right $ confTarget conf
  src <- parseFormat srcName
  trg <- parseFormat trgName
  let outH = confOutputHandle conf
  pure
    Context
      { target = trg
      , source = src
      , outputHandle = outH
      , commutationFlag = confCommutationFlag conf
      }
 where
  parseFormat s = case toUpper <$> s of
    "COPS" -> Right COPS
    "ARI" -> Right ARI
    _ ->
      Left $
        unlines
          [ "ERROR: '" ++ s ++ "' is not a valid FORMAT"
          , "(Must be one of: " ++ show [minBound .. maxBound :: Format] ++ ")"
          ]

runApp :: Context -> FilePath -> IO ()
runApp config inputFile = do
  fileContents <- Text.readFile inputFile

  problem <- case source config of
    COPS -> parseIO (COPS.toParser COPS.parseProblem) inputFile fileContents
      -- | commutationFlag config ->
      --     parseIO (COPS.toParser COPS.parseCOMProblem) inputFile fileContents
      -- | otherwise ->
      --     parseIO (COPS.toParser COPS.parseProblem) inputFile fileContents
    ARI -> parseIO (ARI.toParser ARI.parseProblem) inputFile fileContents

  doc <- case target config of
    COPS
      | commutationFlag config -> unparseIO unparseCopsCOMProblem problem
      | otherwise -> unparseIO unparseCopsProblem problem
    ARI -> unparseIO unparseAriProblem problem

  hPrint (outputHandle config) doc

  hClose (outputHandle config)

parseIO :: Parser a -> String -> Text -> IO a
parseIO p inpName inp =
  case parse (p <* eof) inpName inp of
    Left err -> do
      putStrLn "Error: invalid input"
      putStr $ errorBundlePretty err
      exitFailure
    Right trs -> return trs

-- | Takes an unparsing function @up@ as an argument and wraps the result in the IO monad
unparseIO :: (a -> Either String (Doc ann)) -> a -> IO (Doc ann)
unparseIO up input = case up input of
  Left err -> ioError $ userError err
  Right copsMsTrs -> return copsMsTrs
