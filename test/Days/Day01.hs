module Days.Day01(tests) where
import Test.Tasty
import Test.Tasty.HUnit


import Day1(part1, part2)

exampleDay01 = [1721,
                979,
                366,
                299,
                675,
                1456]

tests = testGroup "Problem 01" [
    testCase "Empty List" $ do
      Day1.part1 [] @?= 0,
    testCase "Example" $ do
      Day1.part1 exampleDay01 @?= 514579,
    testCase "Part 2" $ do
      Day1.part2 exampleDay01 @?= 241861950
    ]
