import Test.HUnit
import Test.Parse.Rule (ruleTests)
import Test.Parse.Term (termTests)

main :: IO ()
main = do
  putStrLn "Testing term parsing"
  _ <- runTestTT termTests
  putStrLn "Testing rule parsing"
  _ <- runTestTT ruleTests
  putStrLn "Testing complete."
