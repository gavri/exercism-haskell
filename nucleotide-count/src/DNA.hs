{-# LANGUAGE TupleSections #-}
module DNA (nucleotideCounts) where

import Data.Map (Map, insertWith, fromList)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
  | all validateNucleotide xs = Right $ foldr incrementCount initNucleotideCounts xs
  | otherwise = Left "Not a nucleotide"
  where incrementCount x = insertWith (+) x 1
        initNucleotideCounts = fromList $ map (, 0) nucleotides
        validateNucleotide x = x `elem` nucleotides
        nucleotides = ['A', 'C', 'G', 'T']

