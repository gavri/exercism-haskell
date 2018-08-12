module LeapYear (isLeapYear) where

isDivisibleBy m n = mod m n == 0

isLeapYear :: Integer -> Bool
isLeapYear year = (isDivisibleBy year 4 && (not (isDivisibleBy year 100))) || isDivisibleBy year 400
