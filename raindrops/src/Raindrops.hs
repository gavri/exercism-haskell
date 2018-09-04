module Raindrops (convert) where

replaceWithRaindrop n m
  | m == 3 && hasFactor = ("Pling" ++)
  | m == 5 && hasFactor = ("Plang" ++)
  | m == 7 && hasFactor = ("Plong" ++)
  | otherwise  = id
  where hasFactor = n `mod` m == 0

raindrops n = (replaceWithRaindrop n 3 . replaceWithRaindrop n 5 . replaceWithRaindrop n 7) ""

convert :: Int -> String
convert n = if null result then show n else result
  where result = raindrops n
