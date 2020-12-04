import Test.Tasty.HUnit
import Test.Tasty

import Day1
import Day2
import Day3
import Day4
import qualified Data.Text as T
import Data.Maybe
import Text.Megaparsec
import Data.Either.Combinators (rightToMaybe)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [day01, day02, day03, day04]

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


exampleDay4 = unlines [
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
   \byr:1937 iyr:2017 cid:147 hgt:183cm",
  "",
   "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
   \hcl:#cfa07d byr:1929",
   "",
   "hcl:#ae17e1 iyr:2013\n\
   \eyr:2024\n\
   \ecl:brn pid:760753108 byr:1931\n\
   \hgt:179cm",
  "",
   "hcl:#cfa07d eyr:2025 pid:166559648\n\
   \iyr:2011 ecl:brn hgt:59in"
  ]
examplePassports =  [
    [("ecl","gry"),("pid","860033327"),("eyr","2020"),("hcl","#fffffd"),("byr","1937"),("iyr","2017"),("cid","147"),("hgt","183cm")],
    [("iyr","2013"),("ecl","amb"),("cid","350"),("eyr","2023"),("pid","028048884"),("hcl","#cfa07d"),("byr","1929")],
    [("hcl","#ae17e1"),("iyr","2013"),("eyr","2024"),("ecl","brn"),("pid","760753108"),("byr","1931"),("hgt","179cm")],
    [("hcl","#cfa07d"),("eyr","2025"),("pid","166559648"),("iyr","2011"),("ecl","brn"),("hgt","59in")]
  ]


parseIt p input =
  rightToMaybe $ runParser p "test input" input


day04 = testGroup "Problem 04" [
   testGroup "Passport Parser" [
     testCase "Field" $ do
       parseIt Day4.parseField "ecl:gry" @?= Just mempty { ecl = Just "gry" }
    -- testCase "Fields" $ do
      -- parseIt Day4.parsePassport "ecl:gry pid:234234234" @?= Just [("ecl", "gry"), ("pid", "234234234")],
    -- testCase "Many Passports" $ do
      -- Day4.parseAllPassports exampleDay4 @?= examplePassports
   ],
  testCase "Example" $ do
    Day4.part1 exampleDay4 @?= 2,
  testCase "Part 2" $ do
    Day4.part2 exampleDay4 @?= 2
  ]