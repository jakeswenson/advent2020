{-# language OverloadedStrings #-}

module Days.Day08(tests) where

import Text.Megaparsec(parseTest, parse, errorBundlePretty)

import Test.Tasty
import Test.Tasty.HUnit

import Day8(part1, part2, parseInstruction, Instruction(..), jump, Marked(..))

example = unlines [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"
  ]

parseIt parser input =
  case parse parser "test" input of
    Left bundle -> error (errorBundlePretty bundle)
    Right r -> r


nop = NotRun $ NoOp 0
acc :: Integer -> Marked
acc = NotRun . Accumulator
jmp = NotRun . Jump

ran :: Marked -> Marked
ran (NotRun inst) = Ran inst
ran i = i

tests = testGroup "Problem 08" [
  testGroup "Parser" [
    testCase "acc" $ do
      parseIt parseInstruction "acc -4" @?= Accumulator (-4),
    testCase "jmp" $ do
      parseIt parseInstruction "jmp -4" @?= Jump (-4),
    testCase "nop" $ do
      parseIt parseInstruction "nop +7" @?= NoOp 7
  ],
  testGroup "Interpreter" [
    testCase "Simple: jumpForwards" $ do
      jump 1 ([ acc 3, nop ], []) @?= ([acc 3, nop], [ ran $ jmp 1]),

    testCase "Simple: jumpBackwards" $ do
      jump (-1) ([ ], [nop, acc 3]) @?= ([nop, ran $ jmp (-1)], [acc 3]),

    testCase "jumpForwards" $ do
      jump 2 ([ acc 3, nop ], []) @?= ([nop], [acc 3, ran $ jmp 2]),

    testCase "jumpBackwards" $ do
      jump (-2) ([ ], [nop, acc 3]) @?= ([acc 3, nop, ran $ jmp (-2)], []),

    testCase "Example: jumpForwards" $ do
      jump 4 ([
          acc 3,
          jmp (-3),
          acc (-99),
          acc 1
        ], [  acc 1, nop ]) @?= ([
          acc 1
        ],
        [
          acc (-99),
          jmp (-3),
          acc 3,
          ran$ jmp 4,
          acc 1,
          nop
        ])

  ],
  testCase "Example" $ do
    part1 example @?= 5,
  testCase "Part 2" $ do
    part2 example @?= Just 8
  ]
