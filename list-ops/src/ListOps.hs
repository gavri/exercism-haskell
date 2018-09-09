module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x: xs) = fzx `seq` foldl' f fzx xs
        where fzx = f z x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x: xs) = f x (foldr f z xs)

length :: [a] -> Int
length [] = 0
length (x: xs) = 1 + length xs

reverse :: [a] -> [a]
reverse xs = reverseIter xs []
  where reverseIter [] acc = acc
        reverseIter (y: ys) acc = reverseIter ys (y: acc)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x: xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x: xs)
  | p x = x: filter p xs
  | otherwise = filter p xs

(++) :: [a] -> [a] -> [a]
xs ++ ys = reverse xs +++ ys
  where [] +++ ys = ys
        (x: xs) +++ ys = xs +++ (x: ys)

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss
