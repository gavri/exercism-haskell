module ProteinTranslation(proteins) where

import Data.Maybe

codonMapping "AUG" = Just "Methionine"
codonMapping "UUU" = Just "Phenylalanine"
codonMapping "UUC" = Just "Phenylalanine"
codonMapping "UUA" = Just "Leucine"
codonMapping "UUG" = Just "Leucine"
codonMapping "UCU" = Just "Serine"
codonMapping "UCC" = Just "Serine"
codonMapping "UCA" = Just "Serine"
codonMapping "UCG" = Just "Serine"
codonMapping "UAU" = Just "Tyrosine"
codonMapping "UAC" = Just "Tyrosine"
codonMapping "UGU" = Just "Cysteine"
codonMapping "UGC" = Just "Cysteine"
codonMapping "UGG" = Just "Tryptophan"
codonMapping "UAA" = Nothing
codonMapping "UAG" = Nothing
codonMapping "UGA" = Nothing

codonChunks [] = []
codonChunks (a: b: c: rest) = [a, b, c]: codonChunks rest

proteins :: String -> Maybe [String]
proteins input = sequence $ takeWhile isJust $ map codonMapping $ codonChunks input
