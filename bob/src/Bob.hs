module Bob (responseFor) where

import Data.Char
import Data.List

isQuestion sentence = (last sentence) == '?'
isYelledOut sentence = (not (any isLower sentence)) && (any isUpper sentence)
isYelledQuestion sentence = isQuestion sentence && isYelledOut sentence
isSilence = all isSpace

responseFor :: String -> String
responseFor sentence
  | isSilence trimmedSentence = "Fine. Be that way!"
  | isYelledQuestion trimmedSentence = "Calm down, I know what I'm doing!"
  | isYelledOut trimmedSentence = "Whoa, chill out!"
  | isQuestion trimmedSentence = "Sure."
  | otherwise = "Whatever."
  where
    trimmedSentence = (dropWhile isSpace . dropWhileEnd isSpace) sentence
