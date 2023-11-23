{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TRSConversion.Formats.CPF3.Unparse.Problem (
  problemToXML,
)
where

import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import Prettyprinter (Pretty, pretty)
import Text.Hamlet.XML (xml)
import Text.XML (Document (..), Element (..), Node, Prologue (..))

import TRSConversion.Problem.CTrs.CTrs (CTrs, Condition ((:==)))
import qualified TRSConversion.Problem.CTrs.CTrs as CTrs
import TRSConversion.Problem.CTrs.Infeasibility (Infeasibility (..))
import qualified TRSConversion.Problem.Problem as Prob
import TRSConversion.Problem.Trs.Trs (Rule (..), Sig (..), Term (..), Trs (..), TrsSig (..))

problemToXML :: (Pretty f, Pretty v) => Prob.Problem f v s -> Document
problemToXML prob = Document (Prologue [] Nothing []) root []
 where
  root = Element "input" Map.empty $ case Prob.system prob of
    Prob.Trs trss -> trssToNodes trss
    Prob.Infeasibility inf -> infToNodes inf
    _ -> error "CPF3 currently only supports TRSs, two-TRSs, and infeasibility problems"

-- One or two trss

trssToNodes :: (Pretty f, Pretty v) => Trs f v -> [Node]
trssToNodes trs@Trs{signature = sig, rules = rs}
  | numSystems trs == 2 = twoTrsToNodes sig (rs IntMap.! 1) (rs IntMap.! 2)
  | numSystems trs == 1 = singleTrsToNodes sig (rs IntMap.! 1)
  | otherwise = error "CPF3 does not support output of problems with more than 2 trss"

singleTrsToNodes :: (Pretty f, Pretty v) => TrsSig f -> [Rule f v] -> [Node]
singleTrsToNodes sig rs =
  [xml|
<trsWithSignature>
  ^{signatureToNodes sig}
  <trs>
    <rules>
      $forall rule <- rs
        ^{ruleToNodes rule}
|]

twoTrsToNodes :: (Pretty f, Pretty v) => TrsSig f -> [Rule f v] -> [Rule f v] -> [Node]
twoTrsToNodes sig r1 r2 =
  [xml|
<twoTrsWithSignature>
  ^{signatureToNodes sig}
  <trs>
    <rules>
      $forall rule <- r1
        ^{ruleToNodes rule}
  <trs>
    <rules>
      $forall rule <- r2
        ^{ruleToNodes rule}
|]

signatureToNodes :: (Pretty f) => TrsSig f -> [Node]
signatureToNodes (FunSig sig) =
  [xml|
<signature>
  $forall Sig f n <- sig
    <symbol>
      <name>#{pack (show (pretty f))}
      <arity>#{pack (show n)}
|]

ruleToNodes :: (Pretty f, Pretty v) => Rule f v -> [Node]
ruleToNodes Rule{lhs = l, rhs = r} =
  [xml|
<rule>
  ^{termToNodes l}
  ^{termToNodes r}
|]

termToNodes :: (Pretty f, Pretty v) => Term f v -> [Node]
termToNodes (Var v) = [xml| <var>#{pack (show (pretty v))} |]
termToNodes (Fun f ts) =
  [xml|
<funapp>
  <name>#{pack (show (pretty f))}
  $forall t <- ts
    ^{termToNodes t}
|]

-- Infeasibility

infToNodes :: (Pretty f, Pretty v) => Infeasibility f v -> [Node]
infToNodes inf =
  [xml|
^{ctrsToNodes (ctrs inf)}
^{queryToNodes (query inf)}
|]

ctrsToNodes :: (Pretty f, Pretty v) => CTrs f v -> [Node]
ctrsToNodes sys
  | CTrs.conditionType sys /= CTrs.Oriented = error "CPF3 only supports ORIENTED ctrss"
  | CTrs.numSystems sys /= 1 = error "CPF3 only supports problems with a single CTRS"
  | otherwise =
      let rs = CTrs.rules sys IntMap.! 1
       in [xml|
<ctrs>
  <conditionType>
    <oriented>
  <rules>
    $forall rule <- rs
      ^{cRuleToNodes rule}
|]

cRuleToNodes :: (Pretty f, Pretty v) => CTrs.CRule f v -> [Node]
cRuleToNodes CTrs.CRule{CTrs.lhs = l, CTrs.rhs = r, CTrs.conditions = conds} =
  [xml|
<rule>
  ^{termToNodes l}
  ^{termToNodes r}
  <conditions>
    $forall cond <- conds
      ^{conditionToNodes cond}
|]

conditionToNodes :: (Pretty f, Pretty v) => CTrs.Condition f v -> [Node]
conditionToNodes (l :== r) =
  [xml|
<condition>
  ^{termToNodes l}
  ^{termToNodes r}
|]

queryToNodes :: (Pretty f, Pretty v) => [Condition f v] -> [Node]
queryToNodes conditions =
  [xml|
<infeasibilityQuery>
  <rules>
    $forall cond <- conditions
      ^{queryCond cond}
|]
 where
  queryCond (l :== r) =
    [xml|
<rule>
  ^{termToNodes l}
  ^{termToNodes r}
|]
