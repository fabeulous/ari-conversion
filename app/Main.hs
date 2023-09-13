{-# LANGUAGE  OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.Char (toUpper)
import qualified Data.Text.IO as Text
import Data.Version (showVersion)
import Paths_trs_conversion (version)
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
import System.IO (
  Handle,
  IOMode (WriteMode),
  hClose,
  hPrint,
  hPutStrLn,
  openFile,
  stderr,
  stdout,
 )

import qualified TRSConversion.Parse.ARI.Problem as ARI
import qualified TRSConversion.Parse.ARI.Utils as ARI
import qualified TRSConversion.Parse.COPS.Problem as COPS
import qualified TRSConversion.Parse.COPS.Utils as COPS
import TRSConversion.Parse.Utils (parseIO)
import TRSConversion.Unparse.Problem (
  unparseAriProblem,
  unparseCopsCOMProblem,
  unparseCopsProblem,
 )

data Format
  = COPS
  | ARI
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | @Config@ holds the information parsed from the options given on the command line.
data Config = Config
  { confTarget :: Maybe String
  , confSource :: Maybe String
  , confCommutationFlag :: Bool
  , confOutputFile :: Maybe FilePath
  }

defaultConfig :: Config
defaultConfig =
  Config
    { confSource = Nothing
    , confTarget = Nothing
    , confCommutationFlag = False
    , confOutputFile = Nothing
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
          ( \s c -> pure $ c{confOutputFile = Just s}
          )
          "FILE"
      )
      "write output to FILE"
  , Option
      []
      ["commutation"]
      ( NoArg (\c -> pure $ c{confCommutationFlag = True})
      )
      "print problem as a COMMUTATION\nproblem in the COPS format"
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
      ["version"]
      ( NoArg
          ( \_ -> do
              execName <- getProgName
              putStrLn (execName ++ " " ++ showVersion version)
              exitSuccess
          )
      )
      "print version"
  ]


usage :: Handle -> IO ()
usage handle = do
  execName <- getProgName
  hPutStrLn handle $ description execName
  hPutStrLn handle (usageInfo "OPTIONS" options)
 where
  description execName =
   unlines
      [ "Usage: " <> execName <> " -f FORMAT -t FORMAT [OPTIONS] FILE"
      , mempty
      , "Convert problems involving term-rewrite systems between formats."
      , mempty
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
    hPutStrLn stderr $ "Error(s) parsing arguments:\n" ++ concat errs
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
  , outputFile :: Maybe FilePath
  }

contextFromConfig :: Config -> Either String Context
contextFromConfig conf = do
  srcName <- maybe (Left "Error: missing source format (-f)") Right $ confSource conf
  trgName <- maybe (Left "Error: missing target format (-t)") Right $ confTarget conf
  src <- parseFormat srcName
  trg <- parseFormat trgName
  let outFile = confOutputFile conf
  pure
    Context
      { target = trg
      , source = src
      , outputFile = outFile
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

  outputHandle <- case outputFile config of
    Nothing -> pure stdout
    Just fp -> openFile fp WriteMode

  hPrint outputHandle doc
  hClose outputHandle

-- | Takes an unparsing function @up@ as an argument and wraps the result in the IO monad
unparseIO :: (a -> Either String (Doc ann)) -> a -> IO (Doc ann)
unparseIO up input = case up input of
  Left err -> ioError $ userError err
  Right copsMsTrs -> return copsMsTrs
