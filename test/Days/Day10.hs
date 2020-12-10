{-# language OverloadedStrings #-}

module Days.Day10(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day10(part1, part2)

example = [
    35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]

tests = testGroup "Problem 10" [
  testCase "Example" $ do
    part1 example @?= 20,
  testCase "Part 2" $ do
    part2 example @?= 20
  ]
