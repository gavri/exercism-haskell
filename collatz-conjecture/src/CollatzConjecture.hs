module CollatzConjecture (collatz) where

collatzRecur :: Integer -> Integer -> Integer
collatzRecur n count
  | n == 1 = count
  | even n = collatzRecur (div n 2) (count + 1)
  | odd n = collatzRecur (3 * n  + 1) (count + 1)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just $ collatzRecur n 0
