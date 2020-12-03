module Day3 (
  problem3,
  problem3Part2,
) where

import Control.Monad

isTree :: Char -> Int
isTree c
  | c == '#' = 1
  | otherwise = 0

parseMapRow :: String -> [Int]
parseMapRow = cycle . map isTree

countTrees :: Int -> Int -> [[Int]] -> Int
countTrees right down treeMap = sum $ do
  (row, idx) <- zip treeMap [0..]
  guard $ idx `mod` down == 0
  return $! row !! (idx*right)

problem3 :: [String] -> Int
problem3 = countTrees 3 1 . map parseMapRow

problem3Part2 :: [String] -> Int
problem3Part2 rows = sum [countTrees right down treeMap | (right, down) <- slopes] where
  treeMap = map parseMapRow rows
  slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
