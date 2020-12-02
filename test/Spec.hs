import Test.Tasty.HUnit
import Test.Tasty

import Day1(problem01, problem01Part2)

main :: IO ()
main = defaultMain tests



tests :: TestTree
tests = testGroup "All Tests" [day01]


exampleDay01 = [1721,
                979,
                366,
                299,
                675,
                1456]
day01 = testGroup "Problem 01" [
    testCase "Empty List" $ do
      problem01 [] @?= 0,
    testCase "Example" $ do
      problem01 exampleDay01 @?= 514579,
    testCase "Part 2" $ do
      problem01Part2 exampleDay01 @?= 241861950
    ]
