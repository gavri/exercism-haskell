module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ multiplesToLimit factors limit
  where multiplesToLimit factors limit = filter (isMultipleOfAnyOf factors) [1..limit - 1]
        isMultipleOfAnyOf ns m = any (isMultipleOf m) ns
        isMultipleOf m n = mod m n == 0
