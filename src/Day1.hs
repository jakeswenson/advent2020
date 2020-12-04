module Day1 (
  part1,
  part2
) where

import Control.Monad

part1 :: [Integer] -> Integer
part1 [] = 0
part1 values =
  mulPair $ findPair values where
  findPair values = head [ (x, v)  | v <- values, x <- values, x+v == 2020 ]
  mulPair (x, y) = x * y

part2 :: [Integer] -> Integer
part2 [] = 0
part2 values = head $ do
  x <- values
  y <- values
  z <- values
  guard $ x + y + z == 2020
  return $ x * y * z
