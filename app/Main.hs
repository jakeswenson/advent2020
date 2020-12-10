module Main where

import System.IO
import Debug.Trace

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

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
  printProblem "day03.1" $ Day3.part1 problem3Map
  printProblem "day03.2" $ Day3.part2 problem3Map

  problem4 <- readFile "problems/day-04.txt"
  printProblem "day04.1" $ Day4.part1 problem4
  printProblem "day04.2" $ Day4.part2 problem4

  problem5 <- readProblemLines "day-05.txt"
  printProblem "day05.1" $ Day5.part1 problem5
  printProblem "day05.2" $ Day5.part2 problem5

  problem6 <- readFile "problems/day-06.txt"
  printProblem "day06.1" $ Day6.part1 problem6
  printProblem "day06.2" $ Day6.part2 problem6

  problem7 <- readFile "problems/day-07.txt"
  printProblem "day07.1" $ Day7.part1 problem7
  printProblem "day07.2" $ Day7.part2 problem7

  problem8 <- readFile "problems/day-08.txt"
  printProblem "day08.1" $ Day8.part1 problem8
  printProblem "day08.2" $ Day8.part2 problem8

  problem9 <- ints <$> readProblemLines "problems/day-09.txt"
  printProblem "day09.1" $ Day9.part1 problem9
  printProblem "day09.2" $ Day9.part2 problem9

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
