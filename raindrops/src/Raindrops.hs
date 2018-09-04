module Raindrops (convert) where

raindrops n = (replaceWithRaindrop 3 . replaceWithRaindrop 5 . replaceWithRaindrop 7) ""
  where
    replaceWithRaindrop m = if hasFactor then (raindropMapping m ++) else id
      where hasFactor = n `mod` m == 0
            raindropMapping 3 = "Pling"
            raindropMapping 5 = "Plang"
            raindropMapping 7 = "Plong"

convert :: Int -> String
convert n = if null result then show n else result
  where result = raindrops n
