module Data.Conversion.Parser.Unparse.ARI
  ( unparseTrsAri
  )
where

import Data.Rewriting.Problem.Type
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import Data.Conversion.Problem.Rule 
import Prettyprinter
 
unparseTrsAri ::  (Pretty f, Pretty v) => Problem f v -> Doc ann
unparseTrsAri prob = block "RULES" (ppRules $ rules prob)
    where
        ppRules rp = undefined -- align $ vcat ([pretty r | r <- strictRules rp])



block n pp = hang 3 $ pretty (n ++ ":") <+> pp
 


{-
instance (Eq f, Eq v, Pretty f, Pretty v) => Pretty (Problem f v) where
  pretty = prettyProblem pretty pretty


{-
**** ARI format
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

printWhen :: Bool -> Doc ann -> Doc ann
printWhen False _ = empty
printWhen True  p = p

text :: String -> String
text = id

prettyWST' :: (Pretty f, Pretty v) => Problem f v -> Doc ann
prettyWST' = prettyWST pretty pretty

prettyWST :: (f -> Doc ann) -> (v -> Doc ann) -> Problem f v -> Doc ann
prettyWST fun var prob =
    printWhen (sterms /= AllTerms) (block "STARTTERM" $ text "CONSTRUCTOR-BASED")
    <> printWhen (strat /= Full) (block "STRATEGY" $ ppStrat strat)
    <> maybeblock "THEORY" theory ppTheories
    <> block "VAR" (ppVars $ variables prob)
    <> maybeblock "SIG" signature ppSignature
    <> block "RULES" (ppRules $ rules prob)
    <> maybeblock "COMMENT" comment text

  where block n pp = (parens $ (hang 3 $ text n <$$> pp) <> linebreak) <> linebreak
        maybeblock n f fpp = case f prob of
                               Just e -> block n (fpp e)
                               Nothing -> empty

        ppStrat Innermost = text "INNERMOST"
        ppStrat Outermost = text "OUTERMOST"

        ppVars vs = align $ fillSep [ var v | v <- vs]

        ppTheories thys = align $ vcat [ppThy thy | thy <- thys]
            where ppThy (SymbolProperty p fs) = block p (align $ fillSep [ fun f | f <- fs ])
                  ppThy (Equations rs)        = block "EQUATIONS" $ vcat [ppRule "==" r | r <- rs]

        ppSignature sigs = align $ fillSep [ppSig sig | sig <- sigs]
            where ppSig (f,i) = parens $ fun f <+> int i

        ppRules rp = align $ vcat ([ppRule "->" r | r <- strictRules rp]
                                   ++ [ppRule "->=" r | r <- weakRules rp])

        ppRule sep = prettyRule (text sep) fun var

        sterms = startTerms prob
        strat  = strategy prob
        thry   = theory prob


prettyProblem :: (Eq f, Eq v) => (f -> Doc ann) -> (v -> Doc ann) -> Problem f v -> Doc ann
prettyProblem fun var prob =  block "Start-Terms" (ppST `on` startTerms)
                              <$$> block "Strategy" (ppStrat `on` strategy)
                              <$$> block "Variables" (ppVars `on` variables)
                              <$$> block "Function Symbols" (ppSyms `on` symbols)
                              <$$> maybeblock "Theory" ppTheories theory
                              <$$> block "Rules" (ppRules `on` rules)
                              <$$> maybeblock "Comment" ppComment comment where
  pp `on` fld = pp $ fld prob
  block n pp = hang 3 $ (underline $ text $ n ++ ":") <+> pp
  maybeblock n pp f = printWhen (isJust `on` f) (block n (pp `on` (fromJust . f)))
  commalist  = fillSep . punctuate (text ",")

  ppST AllTerms      = text "all"
  ppST BasicTerms    = text "basic terms"
  ppStrat Innermost  = text "innermost"
  ppStrat Outermost  = text "outermost"
  ppStrat Full       = text "full rewriting"
  ppVars vars        = commalist $ [var v | v <- nub vars]
  ppSyms syms        = commalist $ [fun v | v <- nub syms]
  ppComment c        = text c
  ppTheories ths     =  align $ vcat [ ppTheory th | th <- ths ] where
      ppTheory (SymbolProperty p fs) = text (p++":") <+> align (commalist [ fun f | f <- fs])
      ppTheory (Equations rs)        = align $ vcat [ppRule "==" r | r <- rs]
  ppRules rp         = align $ vcat $
                       [ppRule "->" r | r <- strictRules rp]
                       ++ [ppRule "->=" r | r <- weakRules rp]
  ppRule sep         = undefined --prettyRule ({-text-} sep) fun var

--instance (Eq f, Eq v, Pretty f, Pretty v) => Pretty (Problem f v) where
--  pretty = prettyProblem pretty pretty
-}