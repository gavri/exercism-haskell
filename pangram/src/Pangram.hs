module Pangram (isPangram) where

import Data.List
import Data.Char

isPangram :: String -> Bool
isPangram text = (nub . sort) (map toLower textWithoutWhiteSpace) == lettersOfTheAlphabetInOrder
  where
    textWithoutWhiteSpace = filter isLetter text
    lettersOfTheAlphabetInOrder =  ['a'..'z']
