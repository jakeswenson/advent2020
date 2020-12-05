module Days.Day05(tests) where
import Test.Tasty
import Test.Tasty.HUnit

import Day5(part1, part2, parseRow, parseChair)

examplesDay5 = [
    ("BFFFBBFRRR", 70, 7, 567),
    ("FFFBBBFRRR", 14, 7, 119),
    ("BBFFBBFRLL", 102, 4, 820)
  ]

ticket (t, _, _, _) = t

tests = testGroup "Problem 05" [
  testGroup "Parser" [
    testCase "Example" $ do
      parseRow "FBFBBFFRLR" @?= 44,
    testCase "Row" $ do
      parseRow "BFFFBBFRRR" @?= 70,
    testCase "Chair" $ do
      parseChair "BFFFBBFRRR" @?= 7
   ],
  testCase "Example" $ do
    part1 (map ticket examplesDay5) @?= 820,
  testCase "Part 2" $ do
    part2 [
       "BBFFBBFRLL",
       "BBFFBBFRRL"
      ] @?= 821
  ]