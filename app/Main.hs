module Main where

import System.IO
import Debug.Trace

import Day1

main :: IO ()
main = do
  problemLines <- readProblemLines "day-01.txt"

  let day1Input = ints problemLines
  let answer = problem01 day1Input
  printProblem "day01.1" $ show . problem01 $ day1Input
  printProblem "day01.2" $ show . problem01Part2 $ day1Input

readProblemLines :: String -> IO [String]
readProblemLines file = do
  contents <- readFile ("problems/" ++ file)
  return $ lines contents


printProblem :: String -> String -> IO ()
printProblem problem result = do
  putStrLn (problem ++ " = " ++ result)


ints :: [String] -> [Integer]
ints = map (read :: String -> Integer)