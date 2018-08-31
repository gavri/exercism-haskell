module Scrabble (scoreLetter, scoreWord) where

import Data.Char

scoreLetter :: Char -> Integer
scoreLetter unnormalizedLetter
  | letter `elem` ['a', 'e', 'i', 'o', 'u', 'l', 'n', 'r', 's', 't'] = 1
  | letter `elem` ['d', 'g'] = 2
  | letter `elem` ['b', 'c', 'm', 'p'] = 3
  | letter `elem` ['f', 'h', 'v', 'w', 'y'] = 4
  | letter `elem` ['k'] = 5
  | letter `elem` ['j', 'x'] = 8
  | letter `elem` ['q', 'z'] = 10
  | otherwise = 0
  where letter = toLower unnormalizedLetter

scoreWord :: String -> Integer
scoreWord word = sum $ map scoreLetter word
