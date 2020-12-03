{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Day2 (
  problem2,
  problem2Part2,
  isValidPassword,
  parseProblem,
  PasswordAndPolicy(..)
) where

import Control.Monad
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Text as T
import Data.Text (splitOn, count, Text)
import Text.Megaparsec
import Text.Megaparsec.Char (char, lowerChar, space1)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void
import Data.Char(isAlpha)

type Parser = Parsec Void Text

data PasswordAndPolicy = PasswordAndPolicy
  { min, max :: Int
  , required :: Char
  , password :: Text
  } deriving (Show, Eq)

-- Parse a line like
--   3-14 w: wwwwwwhwwwwwwpwwwwwf
parseLine :: Parser PasswordAndPolicy
parseLine = do
    min <- decimal
    char '-'
    max <- decimal
    space1
    required <- lowerChar
    char ':'
    space1
    password <- takeWhile1P (Just "password") isAlpha
    return $ PasswordAndPolicy { .. }

parseProblem :: String -> Maybe PasswordAndPolicy
parseProblem = rightToMaybe . runParser parseLine "input" . T.pack

isValidPassword :: PasswordAndPolicy -> Bool
isValidPassword PasswordAndPolicy { .. } =
  let occurrences = T.count (T.singleton required) password
  in occurrences >= min && occurrences <= max

problem2 :: [String] -> Int
problem2 = length . filter isValidPassword . mapMaybe parseProblem

isValidPassword2 :: PasswordAndPolicy -> Bool
isValidPassword2 PasswordAndPolicy { .. } =
  required == firstChar && required /= secondChar || required == secondChar && required /= firstChar
  where
    firstChar = T.index password $ min - 1
    secondChar = T.index password $ max - 1

problem2Part2 :: [String] -> Int
problem2Part2 = length . filter isValidPassword . mapMaybe parseProblem
