{-# LANGUAGE OverloadedStrings #-}
module Day2 (
  problem2,
  problem2Part2,
  isValidPassword,
  parseProblem,
  PasswordAndPolicy(..)
) where

import Control.Monad
import Data.Either
import Data.Text(splitOn, Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char(isAlpha)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void [Char]

data PasswordAndPolicy = PasswordAndPolicy {
   min, max :: Integer
  , required :: String
  , password :: String
  } deriving (Show, Eq)

-- Parse a line like
--   3-14 w: wwwwwwhwwwwwwpwwwwwf
parseLine :: Parser PasswordAndPolicy
parseLine = do
  minLen <- integer
  char '-'
  maxLen <- integer
  space
  required <- lowerChar
  char ':'
  space
  password <- takeWhile1P (Just "password") isAlpha
  return $ PasswordAndPolicy {
    Day2.min = minLen,
    Day2.max = maxLen,
    required = required:[],
    password = password }
  where
    lexeme  = L.lexeme space
    integer = lexeme L.decimal


parseProblem :: String -> PasswordAndPolicy
parseProblem line =
  fromRight (error $ "Failed to Parse line: " ++ line) $ runParser parseLine "input" line

isValidPassword :: PasswordAndPolicy -> Bool
isValidPassword PasswordAndPolicy{Day2.min=min, Day2.max=max, required=required, password=password} =
  let occurrences = toInteger . length $ filter (((==) . head) required) password in
    min <= occurrences && max >= occurrences


countValid :: (PasswordAndPolicy -> Bool) -> [PasswordAndPolicy] -> Integer
countValid _ [] = 0
countValid validator all@(password:passwords) =
  toInteger . length $ filter validator all

problem2 :: [String] -> Integer
problem2 lines =
  countValid isValidPassword passwords
  where
    passwords = map parseProblem lines

isValidPassword2 :: PasswordAndPolicy -> Bool
isValidPassword2 PasswordAndPolicy{Day2.min=first, Day2.max=second, required=required, password=password} =
  char == firstChar && char /= secondChar || char == secondChar && char /= firstChar
  where
    char = head required
    firstChar = password !! ((fromInteger first) - 1)
    secondChar = password !! ((fromInteger second) - 1)

problem2Part2 :: [String] -> Integer
problem2Part2 lines =
  countValid isValidPassword2 passwords
  where
    passwords = map parseProblem lines
