import Test.HUnit
import Test.Parse.Rule (copsRuleTests, ariRuleTests)
import Test.Parse.Term (termTests, prefixTermTests)
import Test.Parse.Trs.Cops (parseCopsTests)
import Test.Parse.Trs.Sig (sigTests)

-- | The testing entry point. Runs each test in turn and logs output to the console.
main :: IO ()
main = do
  putStrLn "Testing term parsing in applicative format"
  _ <- runTestTT termTests
  putStrLn "Testing term parsing in prefix format"
  _ <- runTestTT  prefixTermTests
  putStrLn "Testing parsing COPS rules"
  _ <- runTestTT copsRuleTests
  putStrLn "Testing parsing ARI rules"
  _ <- runTestTT ariRuleTests
  putStrLn "Testing signature parsing"
  _ <- runTestTT sigTests
  putStrLn "Testing parsing TRSs in COPS format"
  _ <- runTestTT parseCopsTests
  putStrLn "Testing complete."
