{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TRSConversion.Parse.COPS.MSTrs
-- Description : Functions to parse MSTRSs
--
-- This module defines functions to parse a many-sorted TRS in COPS and ARI format.
module TRSConversion.Parse.COPS.MSTrs
  ( -- ** COPS
    parseCopsMsTrs,
  )
where

import TRSConversion.Parse.COPS.MsSig (parseCopsMsSigs)
import TRSConversion.Parse.COPS.Rule (parseCopsMsTrsRules)
import TRSConversion.Parse.COPS.Utils (COPSParser, block)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs (..))

-- | Parse a many-sorted TRS in [COPS format](http://project-coco.uibk.ac.at/problems/mstrs.php):
-- see the COCO website for details on the grammar and the tests for more examples.
--
-- Does not carry out type-checking for function applications: this should be handled by the user.
--
-- Leading and trailing spaces are consumed.
-- Note that the entire input will not necessarily be consumed: use `<* eof` if this is needed.
parseCopsMsTrs :: COPSParser (MsTrs String String String)
parseCopsMsTrs = do
  msSigs <- block "SIG" parseCopsMsSigs
  rs <- block "RULES" (parseCopsMsTrsRules msSigs)
  return $
    MsTrs
      { rules = rs,
        signature = msSigs,
        sorts = Nothing -- Not set for COPS format
      }

