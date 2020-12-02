module Main where

import System.IO
import Debug.Trace

import Day1

main :: IO ()
main = do
  contents <- readFile "problems/day-01.txt"
  let problem_lines = lines contents
  let values = map (read :: String -> Integer) problem_lines

  let answer = problem01 values

  putStrLn ("day01.1 = " ++ show answer)
  let answer2 = problem01Part2 values
  putStrLn ("day01.2 = " ++ show answer2)

