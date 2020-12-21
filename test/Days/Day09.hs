{-# language OverloadedStrings #-}

module Days.Day09(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Day9(part1, part2, window)

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

tests = testGroup "Problem 09" [
  testCase "Window" $ do
    window 5 example @?= 127,
  testCase "Example" $ do
    part1 example @?= 20,
  testCase "Part 2" $ do
    part2 example @?= 20
  ]
