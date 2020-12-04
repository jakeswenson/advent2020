module Day3 (
  problem3,
  problem3Part2,
) where

isTree :: Char -> Bool
isTree = (==) '#'

parseMapRow :: String -> [Bool]
parseMapRow = map isTree

retainEvery :: Int -> [a] -> [a]
retainEvery count =
  retain 0 -- start off keeping the first element
  where
    every = count - 1
    retain _ [] = []
    retain 0 (x:xs) = x : retain every xs
    retain i (x:xs) = retain (i-1) xs

walk :: (Int, Int) -> [[a]] -> [a]
walk (dx, dy) treeMap =
  drop 1 $ walkX 0 retainedMap
  where
    retainedMap = retainEvery dy treeMap
    walkX :: Int -> [[a]] -> [a]
    walkX _ [] = []
    walkX xPos (row:rows) =
      (head $ drop xPos $ cycle row) : (walkX (xPos + dx) rows)

countTrees :: (Int, Int) -> [[Bool]] -> Int
countTrees slope =
  length . filter id . walk slope

problem3 :: [String] -> Int
problem3 = countTrees (3, 1) . map parseMapRow

problem3Part2 :: [String] -> Int
problem3Part2 rows =
  product $ map (\ f -> f treeMap) $ map countTrees slopes
  where
    treeMap = map parseMapRow rows
    slopes = [
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2)
      ]