{-# language OverloadedStrings #-}

module Days.Day07(tests) where
import Test.Tasty
import Test.Tasty.HUnit

import Day7(part1, part2, parseBagName, parseBagSpec, bagEdge, BagName(..), Bag(..), BagEdge(..))
import Text.Megaparsec(parseMaybe, parseTest)
import qualified Data.Text as T

example = unlines [
    "light red bags contain 1 bright white bag, 2 muted yellow bags.",
     "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
     "bright white bags contain 1 shiny gold bag.",
     "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
     "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
     "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
     "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
     "faded blue bags contain no other bags.",
     "dotted black bags contain no other bags."
  ]

example2 = unlines [
    "shiny gold bags contain 2 dark red bags.",
    "dark red bags contain 2 dark orange bags.",
    "dark orange bags contain 2 dark yellow bags.",
    "dark yellow bags contain 2 dark green bags.",
    "dark green bags contain 2 dark blue bags.",
    "dark blue bags contain 2 dark violet bags.",
    "dark violet bags contain no other bags."
  ]

parse p = parseTest p . T.pack
testParser p = parseMaybe p . T.pack

tests = testGroup "Problem 06" [
  testGroup "Parsing" [
    testCase "Bag Name" $ do
      parse parseBagName "light red",
    testCase "Edge" $ do
      parse bagEdge "3 light red bags",
    testCase "Bag" $ do
      parse parseBagSpec "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
      testParser parseBagSpec "dark orange bags contain 3 bright white bags, 4 muted yellow bags." @?= (
          Just $ Bag (BagName "dark orange") $ [
              (BagEdge (BagName "dark orange") 3 (BagName "bright white")),
              (BagEdge (BagName "dark orange") 4 (BagName "muted yellow"))
            ]
        ),
    testCase "Bag Name" $ do
      testParser parseBagName "light red" @?= (Just (BagName "light red"))
  ],
  testCase "Example" $ do
    part1 example @?= 4,
  testCase "Part 2 - First example" $ do
    part2 example @?= 32,
  testCase "Part 2 - Second example" $ do
    part2 example2 @?= 126
  ]
