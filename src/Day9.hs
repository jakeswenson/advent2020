module Day9 (
  part1,
  part2,
  window
) where

import qualified Data.Vector as V
import Data.Maybe(mapMaybe)
import Data.Maybe
import Control.Applicative ((<|>))


search :: Integer -> V.Vector Integer -> Maybe Integer
search _ v | V.null v = Nothing
search target vec =
  V.find (== (target - x)) rest <|> search target rest
  where
    x = V.head vec
    rest = V.tail vec

window :: Int -> [Integer] -> Integer
window size input =
  head $ mapMaybe searchZip (zip slices values)
  where
    vec = V.fromList input

    searchZip :: (V.Vector Integer, Integer) -> Maybe Integer
    searchZip (window, targetValue) = search targetValue window

    slices :: [V.Vector Integer]
    slices = [V.slice idx size vec | idx <- [0..V.length vec - size]]

    values = [ vec V.! v | v <- [size .. V.length vec] ]



part1 :: [Integer] -> Int
part1 = length

part2 :: [Integer] -> Int
part2 = length
