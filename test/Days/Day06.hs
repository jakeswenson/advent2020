module Days.Day06(tests) where
import Test.Tasty
import Test.Tasty.HUnit

import Day6(part1, part2)

example = unlines [
    "abc",
    "",
    "a",
    "b",
    "c",
    "",
    "ab",
    "ac",
    "",
    "a",
    "a",
    "a",
    "a",
    "",
    "b"
  ]

tests = testGroup "Problem 06" [
  testCase "Example" $ do
    part1 example @?= 11,
  testCase "Part 2" $ do
    part2 example @?= 6
  ]