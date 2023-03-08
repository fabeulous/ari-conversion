-- |
-- Module      : Data.Conversion.Parser.Unparse.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module Data.Conversion.Parser.Unparse.UnparseTrs
  ( unparseCops,
    unparseAri,
  )
where

import Data.Conversion.Parser.Unparse.Problem.MetaInfo (unparseCopsMetaInfo)
import Data.Conversion.Parser.Unparse.Problem.Rule (unparseCopsRules)
import Data.Conversion.Parser.Unparse.Problem.TrsSig (unparseCopsTrsSig)
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Maybe (catMaybes)
import Prettyprinter (Doc, Pretty, vsep)

-- | Unparse a first-order TRS from the internal 'Trs' representation into
-- [COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the 'Trs'.
unparseCops :: (Eq v, Pretty f, Pretty v) => Trs f v -> Doc ann
unparseCops (Trs rs sig meta) =
  vsep $ unparseCopsTrsSig sig rs : catMaybes [unparseCopsRules rs, unparseCopsMetaInfo meta]

-- | qqjf
unparseAri :: (Pretty f, Pretty v) => Trs f v -> Doc ann
unparseAri = undefined

{-
\**** ARI format
(meta-info (origin "COPS #20"))
(meta-info (doi "10.1007/11805618_6"))
(meta-info (comment "[7] Example 2"))
(meta-info (submitted "Takahito Aoto" "Junichi Yoshida" "Yoshihito Toyama"))
(format TRS)
(fun 0 0)
(fun nats 0)
(fun hd 1)
(fun tl 1)
(fun inc 1)
(fun : 2)
(rule nats (: 0 (inc nats)))
(rule (inc (: x y)) (: (s x) (inc y)))
(rule (hd (: x y)) x)
(rule (tl (: x y)) x)
(rule (inc (tl nats))) (tl (inc nats)))
-}