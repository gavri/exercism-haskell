module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
  | yearIsDivisibleBy 400 = True
  | yearIsDivisibleBy 100 = False
  | yearIsDivisibleBy 4 = True
  | otherwise = False
  where
    yearIsDivisibleBy n = mod year n == 0

