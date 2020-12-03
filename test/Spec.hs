import Test.Tasty.HUnit
import Test.Tasty

import Day1(problem01, problem01Part2)
import Day2(problem2, parseProblem, PasswordAndPolicy(..), isValidPassword, problem2Part2)
import Day3(problem3)
import qualified Data.Text as T
import Data.Maybe

main :: IO ()
main = defaultMain tests



tests :: TestTree
tests = testGroup "All Tests" [day01, day02, day03]


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

exampleDay02 = [
  "1-3 a: abcde",
  "1-3 b: cdefg",
  "2-9 c: ccccccccc"]

day02 = testGroup "Problem 02" [
  testGroup "Parsing" [
    testCase "Parse Line" $ do
      let result = parseProblem $ head exampleDay02
        in result @?= Just (PasswordAndPolicy 1 3 'a' $ T.pack "abcde")
  ],
  testGroup "Valid Passwords" [
    testCase "Example 1" $ do
      let password = parseProblem $ head exampleDay02 in
        (isValidPassword . fromJust) password @?= True
  ],
  testCase "Empty List" $ do
    problem2 [] @?= 0,
  testCase "Example" $ do
    problem2 exampleDay02 @?= 2,
  testCase "Part 2" $ do
    problem2Part2 exampleDay02 @?= 1
  ]

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

day03 = testGroup "Problem 03" [
  testCase "Example" $ do
    problem3 exampleDay3 @?= 7
  ]
