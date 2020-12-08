module Days.Day08(tests) where
import Test.Tasty
import Test.Tasty.HUnit

import Day8(part1, part2)

example = unlines [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"
  ]

tests = testGroup "Problem 08" [
  testCase "Example" $ do
    part1 example @?= 5,
  testCase "Part 2" $ do
    part2 example @?= 6
  ]
