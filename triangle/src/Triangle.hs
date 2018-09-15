module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType :: (Ord a, Eq a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | a == 0 && b == 0 && c == 0 = Illegal
  | (firstSide + secondSide) < maxSide = Illegal
  | a == b && b == c  = Equilateral
  | a == b || b == c || c == a = Isosceles
  | otherwise = Scalene
  where
    [firstSide, secondSide] = delete maxSide [a, b, c]
    maxSide = maximum [a, b, c]
