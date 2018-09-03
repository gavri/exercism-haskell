module RunLength (decode, encode) where

import Data.List
import Text.Printf
import Text.ParserCombinators.ReadP
import Text.Read.Lex

groupSizeParser :: ReadP Int
groupSizeParser = readDecP <++ pure 1

groupLetterParser :: ReadP Char
groupLetterParser = get

groupParser :: ReadP String
groupParser = do
  size <- groupSizeParser
  letter <- groupLetterParser
  return $ replicate size letter

groupsParser :: ReadP String
groupsParser = fmap concat $ manyTill groupParser eof

decode :: String -> String
decode encodedText = (fst . head) parsed
  where parsed = readP_to_S groupsParser encodedText

encode :: String -> String
encode text = concatMap encodeGroup grouped
  where grouped = group text

encodeGroup :: String -> String
encodeGroup text
  | size == 1 = text
  | otherwise = printf "%d%c" size (head text)
  where size = length text
