import Test.Tasty.HUnit
import Test.Tasty

import Day1
import Day2
import Day3
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
      Day1.part1 [] @?= 0,
    testCase "Example" $ do
      Day1.part1 exampleDay01 @?= 514579,
    testCase "Part 2" $ do
      Day1.part2 exampleDay01 @?= 241861950
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
    Day2.part1 [] @?= 0,
  testCase "Example" $ do
    Day2.part1 exampleDay02 @?= 2,
  testCase "Part 2" $ do
    Day2.part2 exampleDay02 @?= 1
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
    Day3.part1 exampleDay3 @?= 7,
  testCase "Part 2" $ do
    Day3.part2 exampleDay3 @?= 336
  ]
