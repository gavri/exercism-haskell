module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isEquivalent xs) $ filter (isNotCaseInsensitiveEquivalent xs) xss
  where isEquivalent as bs = anagramEquivalence as == anagramEquivalence bs
        anagramEquivalence ys = sort (caseInsensitiveEquivalence ys)
        isNotCaseInsensitiveEquivalent as bs = caseInsensitiveEquivalence as /= caseInsensitiveEquivalence bs
        caseInsensitiveEquivalence = map toLower
