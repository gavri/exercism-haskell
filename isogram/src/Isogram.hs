module Isogram (isIsogram) where

import Data.List
import Data.Char

isIsogram :: String -> Bool
isIsogram s = nub normalizedS == normalizedS
  where normalizedS = sort $ map toLower $ filter isSignificantChar s
        isSignificantChar c = c /= ' ' && c /= '-'

