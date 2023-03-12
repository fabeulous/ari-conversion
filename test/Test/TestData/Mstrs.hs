-- |
-- Module      : Test.TestData.Mstrs
-- Description : Example data for Mstrs tests
--
-- This module defines test data which is used for testing both parsing and unparsing functions for 'Mstrs's.
-- Exported values can then be imported in 'Test.Parse' and 'Test.Unparse'.
module Test.TestData.Mstrs
  ( -- * Test data for tests on 'Mstrs'
    copsMstrss,
    ariMstrss,
  )
where
 
import Data.Conversion.Problem.Mstrs.Mstrs (Mstrs (..))

--------------------------- MSTRS Lists --------
------------------------

-- | A list of MSTRSs in internal 'Mstrs' format and in (COPS format)[http://project-coco.uibk.ac.at/problems/mstrs.php]
-- to test both parsing and unparsing functions.
-- Has format @(test label, original Mstrs, ARI string, Mstrs after unparsing)@.
--
-- The result for unparsing might differ from the original MSTRS did not specify sorts.
copsMstrss :: [(String, Mstrs String String String, String, Mstrs String String String)]
copsMstrss = []

-- | A list of MSTRSs in internal 'Mstrs' format and in (ARI format)[https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt]
-- to test both parsing and unparsing functions.
-- Has format @(test label, original Mstrs, ARI string, Mstrs after unparsing)@.
--
-- The result for unparsing might differ from the original MSTRS did not specify sorts.
ariMstrss :: [(String, Mstrs String String String, String, Mstrs String String String)]
ariMstrss =
  []

------------------------
--- MSTRSs -------------
------------------------

------------------------
--- Rules --------------
------------------------
