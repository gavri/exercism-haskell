module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

perfectlyDivisible m n = mod m n == 0

factors :: Int -> [Int]
factors m = filter (perfectlyDivisible m) candidates
  where candidates = [1..(div m 2)]

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | sumOfFactors < n = Just Deficient
  | sumOfFactors == n = Just Perfect
  | sumOfFactors > n = Just Abundant
  where sumOfFactors = sum $ factors n
