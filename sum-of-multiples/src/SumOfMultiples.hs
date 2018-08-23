module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter (\m -> any ((== 0) . (mod m)) factors) [1..limit - 1]
