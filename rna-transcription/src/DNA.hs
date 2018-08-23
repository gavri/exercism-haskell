module DNA (toRNA) where

transcribeNucleotide :: Char -> Maybe Char
transcribeNucleotide c = lookup c transcriptionMap
  where transcriptionMap = [('G', 'C'), ('C', 'G'), ('T', 'A'), ('A', 'U')]

toRNA :: String -> Maybe String
toRNA xs = traverse transcribeNucleotide xs
