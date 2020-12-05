module Days.Day03(tests) where
import Test.Tasty
import Test.Tasty.HUnit


import Day3(part1, part2)

exampleDay3 = [
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#"
  ]

tests = testGroup "Problem 03" [
  testCase "Example" $ do
    part1 exampleDay3 @?= 7,
  testCase "Part 2" $ do
    part2 exampleDay3 @?= 336
  ]
