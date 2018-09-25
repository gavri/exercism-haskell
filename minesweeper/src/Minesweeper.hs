module Minesweeper (annotate) where

import Control.Arrow

withIndex xs = zip xs [0..]

map2DWithIndex :: (a -> Int -> Int -> b) -> [[a]] -> [[b]]
map2DWithIndex f grid = map rowMapper rowsWithIndices
  where rowMapper (row, r) = map colMapper colsWithIndices
          where colMapper (cell, c) = f cell r c
                colsWithIndices = withIndex row
        rowsWithIndices = withIndex grid

neighbors :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbors position width height = filter isNeighbor candidateNeighbors
  where candidateNeighbors = sequence [f *** g | f <- [inc, dec, id], g <- [inc, dec, id]] position
        isNeighbor position = notSelf position && onBoard position
        onBoard (x, y) = x >= 0 && x < height && y >= 0 && y < width
        notSelf otherPositon = position /= otherPositon
        inc = (+ 1)
        dec = subtract 1

annotate :: [String] -> [String]
annotate board = map2DWithIndex fill board
        where fill cellContent x y = if hasBomb cellContent then '*' else numberOfBombs x y
              numberOfBombs x y = displayedCell $ length $ filter hasBomb $ map cell $ neighbors (x, y) width height
              width = (length . head) board
              height = length board
              displayedCell n = if n > 0 then digitToChar n else ' '
              cell (x, y) = (board !! x) !! y
              digitToChar = head . show
              hasBomb = (== '*')
