module Raindrops (convert) where

raindrops n = (replaceWithRaindrop 3 . replaceWithRaindrop 5 . replaceWithRaindrop 7) ""
  where
    replaceWithRaindrop 3 = if (hasFactor 3) then ("Pling" ++) else id
    replaceWithRaindrop 5 = if (hasFactor 5) then ("Plang" ++) else id
    replaceWithRaindrop 7 = if (hasFactor 7) then ("Plong" ++) else id
    hasFactor m = n `mod` m == 0

convert :: Int -> String
convert n = if null result then show n else result
  where result = raindrops n
