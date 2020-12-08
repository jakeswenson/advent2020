{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Day8 (
  part1,
  part2,
) where


import Control.Monad
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Data.Either(fromRight)
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char (char, lowerChar, space, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void
import Data.Char(isAlpha)

data Instruction =
  | Accumulator Int
  | Jump Int
  | NoOp

type Program = [Instruction]

part1 :: String -> Int
part1 = length

part2 :: String -> Int
part2 = length
