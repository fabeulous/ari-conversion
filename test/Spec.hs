import Test.Parse.Spec (parsingTests)
import Test.Unparse.Spec (unparsingTests)

-- | The testing entry point. Runs each test in turn and logs output to the console.
--
-- Run tests by calling `stack test` in the terminal.
main :: IO ()
main = do
  _ <- parsingTests
  _ <- unparsingTests
  putStrLn "Testing complete."