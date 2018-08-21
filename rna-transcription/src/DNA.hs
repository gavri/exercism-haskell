module DNA (toRNA) where

transcribeNucleotide :: Char -> Maybe Char
transcribeNucleotide 'G' = Just 'C'
transcribeNucleotide 'C' = Just 'G'
transcribeNucleotide 'T' = Just 'A'
transcribeNucleotide 'A' = Just 'U'
transcribeNucleotide _ = Nothing

toRNA :: String -> Maybe String
toRNA xs = sequence $ map transcribeNucleotide xs
