module Test.Parse.Trs.Sig (sigTests) where -- qqjf module description

import Data.Conversion.Parser.Parse.Problem.Sig (parseSig)
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Test.HUnit
import Test.Parse.Utils (assertFailParseList, assertParseList)

sigTests :: Test
sigTests = TestList [parseSigTests, badSigTests]

-- | qqjf
parseSigTests :: Test
parseSigTests = assertParseList validSigs parseSig
  where
    validSigs :: [(String, [Sig String])]
    validSigs =
      [ ("(f 1)", [Sig "f" 1]),
        ("(f 1) ", [Sig "f" 1]),
        (" (f 1)", [Sig "f" 1]),
        ("  ( f 1  )", [Sig "f" 1]),
        ("(f 2) (a 0) (h 1)", [Sig "f" 2, Sig "a" 0, Sig "h" 1]),
        ("(f 2) (f 1)", [Sig "f" 2, Sig "f" 1]),
        ("(g 25) (g 25)", [Sig "g" 25, Sig "g" 25])
      ]

-- | qqjf
badSigTests :: Test
badSigTests = assertFailParseList badSigs parseSig
  where
    badSigs :: [String]
    badSigs =
      [ "f 1",
        "f 1 g 2",
        "(f 1 g 2)",
        "h -1",
        "(g (1))",
        "(f 1 2)",
        "(f 1) (h 1"
      ]
