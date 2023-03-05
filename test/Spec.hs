import Test.HUnit
import Test.Parse.Rule (ruleTests)
import Test.Parse.Term (termTests)
import Test.Parse.Trs.Sig (sigTests)

main :: IO ()
main = do
  putStrLn "Testing term parsing"
  _ <- runTestTT termTests
  putStrLn "Testing rule parsing"
  _ <- runTestTT ruleTests
  putStrLn "Testing signature parsing"
  _ <- runTestTT sigTests
  putStrLn "Testing complete."
