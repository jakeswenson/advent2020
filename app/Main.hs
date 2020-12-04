module Main where

import System.IO
import Debug.Trace

import Day1
import Day2
import Day3

main :: IO ()
main = do
  problem1Lines <- readProblemLines "day-01.txt"

  let day1Input = ints problem1Lines
  printProblem "day01.1" $ Day1.part1 day1Input
  printProblem "day01.2" $ Day1.part2 day1Input

  problem2Lines <- readProblemLines "day-02.txt"
  printProblem "day02.1" $ Day2.part1 problem2Lines
  printProblem "day02.2" $ Day2.part2 problem2Lines

  problem3Map <- readProblemLines "day-03.txt"
  printProblem "day03.1" $ Day2.part1 problem3Map
  printProblem "day03.2" $ Day2.part2 problem3Map


readProblemLines :: String -> IO [String]
readProblemLines file = do
  contents <- readFile ("problems/" ++ file)
  return $ lines contents


printProblem :: Show s => String -> s -> IO ()
printProblem problem result = do
  putStrLn $ problem ++ " = " ++ answer
  where
    answer = show result


ints :: [String] -> [Integer]
ints = map (read :: String -> Integer)