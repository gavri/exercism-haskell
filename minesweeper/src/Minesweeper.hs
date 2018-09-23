module Minesweeper (annotate) where

mapFst f (x, y) = (f x, y)
mapSnd = fmap

inc x = x + 1
dec x = x - 1

map2DWithIndex :: (a -> Int -> Int -> b) -> [[a]] -> [[b]]
map2DWithIndex f grid = map (\(row, r) -> (map (\(cell, c) -> f cell r c) (zip row [0..]))) $ zip grid [0..]

neighbors :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbors position width height = filter onBoard candidateNeighbors
  where candidateNeighbors = map ($ position) [mapFst dec,
                                               mapFst inc,
                                               mapSnd dec,
                                               mapSnd inc,
                                               (mapFst inc) . (mapSnd inc),
                                               (mapFst inc) . (mapSnd dec),
                                               (mapFst dec) . (mapSnd inc),
                                               (mapFst dec) . (mapSnd dec)]
        onBoard (x, y) = x >= 0 && x < height && y >= 0 && y < width

annotate :: [String] -> [String]
annotate board = map2DWithIndex fill board
        where fill cellContent x y = if (cellContent == '*') then '*' else numberOfBombs x y
              numberOfBombs x y = displayedCell $ length $ filter (== '*') $ map (\(row, col) -> (board !! row) !! col) $ neighbors (x, y) width height
              width = (length . head) board
              height = length board
              displayedCell n = if (n > 0) then ((show n) !! 0) else ' '
