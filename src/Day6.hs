module Day6 (
  part1,
  part2,
) where

import Data.Set(Set, fromList, member, empty, insert, size, intersection)
import qualified Data.Text as T
import Data.Char(isAlpha)

splitGroups :: String -> [String]
splitGroups =  map T.unpack . T.splitOn (T.pack "\n\n") . T.pack

countUniqueAnswers :: String -> Int
countUniqueAnswers = size . fromList

part1 :: String -> Int
part1 = sum . map countUniqueAnswers . parseInput
  where
    parseInput :: String -> [String]
    parseInput = map (filter isAlpha) . splitGroups

part2 :: String -> Int
part2 =
  sum . map size . intersect . answers
  where
    answers :: String -> [[String]]
    answers = map lines . splitGroups

    answerSets :: [[String]] -> [[Set Char]]
    answerSets = map (map fromList)

    intersect :: [[String]] -> [Set Char]
    intersect = map (foldl1 intersection) . answerSets

