module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs == length ys = Just $ length unEqualPairs
  | otherwise = Nothing
  where unEqualPairs = filter (uncurry (/=)) pairs
        pairs = zip xs ys
