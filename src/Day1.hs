module Day1 (
  problem01,
  problem01Part2
) where

import Control.Monad

problem01 :: [Integer] -> Integer
problem01 [] = 0
problem01 values =
  mulPair $ findPair values where
  findPair values = head [ (x, v)  | v <- values, x <- values, x /= v, x+v == 2020 ]
  mulPair (x, y) = x * y

problem01Part2 :: [Integer] -> Integer
problem01Part2 [] = 0
problem01Part2 values = head $ do
  x <- values
  y <- values
  z <- values
  guard (x + y + z == 2020)
  return (x * y * z)
