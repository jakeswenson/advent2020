module Day3 (
  problem3,
  problem3Part2,
) where

isTree :: Char -> Bool
isTree = (==) '#'

parseMapRow :: String -> [Bool]
parseMapRow = map isTree

countTrees :: Int -> [[Bool]] -> Int
countTrees step treeMap =
  countTreesCols step 0 treeMap
  where
    countTreesCols :: Int -> Int -> [[Bool]] -> Int
    countTreesCols _ _ [] = 0
    countTreesCols slope col (row:rows) =
      (+) tree $ countTreesCols slope (col + slope) rows
      where
        tree =
          if row !! (mod col (length row))
          then 1
          else 0

problem3 :: [String] -> Int
problem3 = countTrees 3 . map parseMapRow

problem3Part2 :: [String] -> Int
problem3Part2 rows =
  twoRows * (product $ map (\ f -> f treeMap) $ map countTrees paths)
  where
    treeMap = map parseMapRow rows
    paths = [
        1,
        3,
        5,
        7
      ]

    twoRows = countTrees 1 $ skipRows treeMap

    skipRows :: [a] -> [a]
    skipRows [] = []
    skipRows (row:[]) = []
    skipRows (row : _: []) = [row]
    skipRows (row:_:rows) = row:(skipRows rows)
