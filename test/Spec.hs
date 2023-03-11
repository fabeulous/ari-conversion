import Test.HUnit
import Test.Parse.MetaInfo (metaInfoParsingTests)
import Test.Parse.Mstrs.Cops (parseCopsMstrsTests)
import Test.Parse.Mstrs.MsSig (msSigTests)
import Test.Parse.Rule (ariRuleTests, copsRuleTests)
import Test.Parse.Term (prefixTermTests, termTests)
import Test.Parse.Trs.Ari (parseAriTests)
import Test.Parse.Trs.Cops (parseCopsTests)
import Test.Parse.Trs.Sig (sigTests)
import Test.Unparse.Problem.MetaInfo (unparseMetaInfoTests)
import Test.Unparse.Problem.Rule (unparseRuleTests)
import Test.Unparse.Problem.Term (unparseTermTests)
import Test.Unparse.Problem.TrsSig (unparseSigTests)
import Test.Unparse.UnparseTrs (unparseAriTrsTests, unparseCopsTrsTests)

-- | The testing entry point. Runs each test in turn and logs output to the console.
main :: IO ()
main = do
  putStrLn "Testing term parsing"
  _ <- runTestTT termTests
  _ <- runTestTT prefixTermTests
  putStrLn "Testing parsing MetaInfo"
  _ <- runTestTT metaInfoParsingTests
  putStrLn "Testing parsing rules"
  _ <- runTestTT copsRuleTests
  _ <- runTestTT ariRuleTests
  putStrLn "Testing signature parsing"
  _ <- runTestTT sigTests
  _ <- runTestTT msSigTests
  putStrLn "Testing parsing TRSs"
  _ <- runTestTT parseCopsTests
  _ <- runTestTT parseAriTests
  putStrLn "Testing parsing MSTRSs"
  _ <- runTestTT parseCopsMstrsTests
  putStrLn "Testing unparsing terms"
  _ <- runTestTT unparseTermTests
  putStrLn "Testing unparsing rules"
  _ <- runTestTT unparseSigTests
  putStrLn "Testing unparsing TRS metadata"
  _ <- runTestTT unparseMetaInfoTests
  putStrLn "Testing unparsing TRS signatures"
  _ <- runTestTT unparseRuleTests
  putStrLn "Testing unparsing TRSs"
  _ <- runTestTT unparseCopsTrsTests
  _ <- runTestTT unparseAriTrsTests
  putStrLn "Testing complete."
