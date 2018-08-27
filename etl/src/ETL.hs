module ETL (transform) where

import Data.Char
import Data.Map (Map, fromList, toList)
import Control.Arrow

transform :: Map a String -> Map Char a
transform legacyData = fromList $ transformList $ toList legacyData

transformList :: [(a, String)] -> [(Char, a)]
transformList legacyData = concatMap (\(i, s) -> map (\c -> (c, i)) s) legacyDataLowerCased
  where legacyDataLowerCased = map (second toLowerString) legacyData
        toLowerString = map toLower
