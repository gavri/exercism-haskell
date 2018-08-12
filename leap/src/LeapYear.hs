module LeapYear (isLeapYear) where

m `isDivisibleBy` n = mod m n == 0

isLeapYear :: Integer -> Bool
isLeapYear year = (year `isDivisibleBy` 4 && (not (year `isDivisibleBy` 100))) || year `isDivisibleBy` 400
