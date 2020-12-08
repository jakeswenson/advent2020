import Test.Tasty
import Test.Tasty.HUnit

import Days.Day01
import Days.Day02
import Days.Day03
import Days.Day04
import Days.Day05
import Days.Day06
import Days.Day07

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All Tests" [
    Days.Day01.tests
    , Days.Day02.tests
    , Days.Day03.tests
    , Days.Day04.tests
    , Days.Day05.tests
    , Days.Day06.tests
    , Days.Day07.tests
  ]
