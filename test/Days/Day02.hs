module Days.Day02(tests) where
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Data.Maybe

import Day2(
  part1
  , part2
  , isValidPassword
  , parseProblem
  , PasswordAndPolicy(..)
  )


exampleDay02 = [
  "1-3 a: abcde",
  "1-3 b: cdefg",
  "2-9 c: ccccccccc"]

tests = testGroup "Problem 02" [
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
    part1 [] @?= 0,
  testCase "Example" $ do
    part1 exampleDay02 @?= 2,
  testCase "Part 2" $ do
    part2 exampleDay02 @?= 1
  ]
