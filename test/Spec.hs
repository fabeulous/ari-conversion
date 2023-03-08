import Test.HUnit
import Test.Parse.MetaInfo (metaInfoParsingTests)
import Test.Parse.Rule (ariRuleTests, copsRuleTests)
import Test.Parse.Term (prefixTermTests, termTests)
import Test.Parse.Trs.Ari (parseAriTests)
import Test.Parse.Trs.Cops (parseCopsTests)
import Test.Parse.Trs.Sig (sigTests)
import Test.Unparse.Problem.MetaInfo (unparseMetaInfoTests)
import Test.Unparse.Problem.Rule (unparseRuleTests)
import Test.Unparse.Problem.Term (unparseTermTests)
import Test.Unparse.Problem.TrsSig (unparseSigTests)
import Test.Unparse.UnparseTrs (unparseCopsTrsTests)

-- | The testing entry point. Runs each test in turn and logs output to the console.
main :: IO ()
main = do
  putStrLn "Testing term parsing in applicative format"
  _ <- runTestTT termTests
  putStrLn "Testing term parsing in prefix format"
  _ <- runTestTT prefixTermTests
  putStrLn "Testing parsing MetaInfo"
  _ <- runTestTT metaInfoParsingTests
  putStrLn "Testing parsing COPS rules"
  _ <- runTestTT copsRuleTests
  putStrLn "Testing parsing ARI rules"
  _ <- runTestTT ariRuleTests
  putStrLn "Testing signature parsing"
  _ <- runTestTT sigTests
  putStrLn "Testing parsing TRSs in COPS format"
  _ <- runTestTT parseCopsTests
  putStrLn "Testing parsing TRSs in ARI format"
  _ <- runTestTT parseAriTests
  putStrLn "Testing unparsing terms"
  _ <- runTestTT unparseTermTests
  putStrLn "Testing unparsing rules"
  _ <- runTestTT unparseSigTests
  putStrLn "Testing unparsing TRS metadata"
  _ <- runTestTT unparseMetaInfoTests
  putStrLn "Testing unparsing TRS signatures"
  _ <- runTestTT unparseRuleTests
  putStrLn "Testing converting TRSs to COPS format"
  _ <- runTestTT unparseCopsTrsTests
  putStrLn "Testing complete."
