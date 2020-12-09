{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Day8 (
  part1,
  part2,
  parseInstruction,
  Instruction(..),
  Marked(..),
  jump
) where


import Control.Monad
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Data.Either(fromRight, rights)
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char (char, lowerChar, space, space1, string)
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Data.Void
import Data.Char(isAlpha)

type Parser = Parsec Void T.Text

data Instruction =
  Accumulator Integer
  | Jump Int
  | NoOp Int
  deriving (Show, Eq, Ord)

data Marked = NotRun Instruction | Ran Instruction
  deriving (Show, Eq, Ord)

parseInstruction :: Parser Instruction
parseInstruction =
    acc <|> jmp <|> nop
  where
    acc = do
      string "acc"
      space1
      adjustment <- signed space decimal
      return $ Accumulator adjustment
    jmp = do
      string "jmp"
      space1
      offset <- fromInteger <$> signed space decimal
      return $ Jump offset
    nop = do
      string "nop"
      space1
      value <- signed space decimal
      return $ NoOp value

jump :: Int -> ([Marked], [Marked]) -> ([Marked], [Marked])
jump o (_, []) | o < 0 = error ("Can't go back without history" ++ show o)
jump o ([], _) | o > 0 = error ("Can't go forward without next: " ++ show o)
jump 0 (next, history) = ((Ran $ Jump 0):next, history)
jump offset (next, history) | offset < 0 =
  doJumpBack offset (inst:next, history)
  where
    inst = Ran $ Jump offset
    doJumpBack 0 a = a
    doJumpBack offset (next, ran:history) =
      doJumpBack (offset + 1) (ran:next, history)

jump offset (next, history) | offset > 0 =
  doJump offset (inst:next, history)
  where
    inst = Ran $ Jump offset
    doJump 0 a = a
    doJump offset (current:next, history) =
      doJump (offset-1) (next, current:history)

type InterpretResult = Integer

runOnce :: Integer -> [Marked] -> [Marked] -> InterpretResult
runOnce a [] _ = a
runOnce acc (current:rest) history =
  case current of
    Ran dupe -> acc
    NotRun inst -> case inst of
        Accumulator adjustment ->
          runOnce (acc + adjustment) rest ((Ran inst):history)
        Jump offset ->
          uncurry (runOnce acc) $ jump offset (rest, history)
        NoOp _ -> runOnce acc rest ((Ran inst):history)

interpret :: [Instruction] -> InterpretResult
interpret instructions =
  runOnce 0 (markNotRun instructions) []
  where
    markNotRun = map NotRun

part1 :: String -> Integer
part1 =
  interpret . instructions
  where
    instructions = rights . map (parse parseInstruction "line") . T.lines . T.pack

fix :: Bool -> Integer -> [Marked] -> [Marked] -> Maybe InterpretResult
fix _ a [] _ = Just a
fix hasReplaced acc (current:rest) history =
  case current of
    Ran dupe -> Nothing
    NotRun inst -> case inst of
        Accumulator adjustment ->
          fix hasReplaced (acc + adjustment) rest ((Ran inst):history)
        Jump offset -> -- backwards
          case uncurry (fix hasReplaced  acc) $ jump offset (rest, history) of
            Nothing | not hasReplaced -> fix True acc ((NotRun $ NoOp offset):rest) history -- Try NoOp
            a -> a
        NoOp offset -> case fix hasReplaced acc rest ((Ran inst):history) of
          Nothing | not hasReplaced -> fix True acc ((NotRun $ Jump offset):rest) history
          a -> a


part2 :: String -> Maybe Integer
part2 input =
  findFixVal $ (instructions input) ++ [Accumulator 0]
  where
    findFixVal :: [Instruction] -> Maybe Integer
    findFixVal insts = fix False 0 (map NotRun insts) []
    instructions = rights  . map (parse parseInstruction "line") . T.lines . T.pack
