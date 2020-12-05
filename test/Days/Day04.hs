module Days.Day04(tests) where
import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec
import Data.Either.Combinators (rightToMaybe)

import Day4(part1, part2, parseField, parsePassport, parseAllPassports)

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

tests = testGroup "Problem 04" [
  testGroup "Passport Parser" [
    testCase "Field" $ do
      parseIt parseField "ecl:gry" @?= Just ("ecl", "gry"),
    testCase "Fields" $ do
      parseIt parsePassport "ecl:gry pid:234234234" @?= Just [("ecl", "gry"), ("pid", "234234234")],
    testCase "Many Passports" $ do
      parseAllPassports exampleDay4 @?= examplePassports
  ],
  testCase "Example" $ do
    part1 exampleDay4 @?= 2,
  testCase "Part 2" $ do
    part2 exampleDay4 @?= 2
  ]

