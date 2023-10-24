{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Unparse.COM (unparseCopsCOM) where

import Prettyprinter (Doc, Pretty (pretty), parens, vsep, (<+>))
import TRSConversion.Problem.Trs.Trs (Trs)
import TRSConversion.Unparse.UnparseTrs (unparseCopsTrs)

unparseCopsCOM :: (Ord f, Ord v, Pretty f, Pretty v) => String -> Trs f v -> Either String (Doc ann)
unparseCopsCOM comment trs = do
    prettySystems <- unparseCopsTrs trs
    pure $
        vsep
            [ parens $ "PROBLEM" <+> "COMMUTATION"
            , parens $ "COMMENT" <+> pretty comment
            , prettySystems
            ]
