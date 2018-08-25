module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n
  | n >= 1 && n <= 64 = Just $ 2 ^ (n - 1)
  | otherwise = Nothing

totalForReadability :: Integer
totalForReadability = sum $ mapMaybe square [1..64]

totalForSpeed :: Integer
totalForSpeed = 18446744073709551615

total :: Integer
total = totalForReadability

