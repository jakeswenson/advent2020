module Day4 (
  part1,
  part2,
  parseField,
  parsePassport,
  parseAllPassports
) where

import Control.Monad
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Text as T
import Data.Text (splitOn, count, Text)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void
import Data.Char(isAlpha, isSpace, isDigit)

type Parser = Parsec Void Text

type PassportField = (String, Text)
type Passport = [PassportField]

parseField :: Parser PassportField
parseField = do
  fieldName <- takeWhile1P (Just "fieldName") isAlpha
  char ':'
  value <- takeWhile1P (Just "fieldValue") (not . isSpace)
  return (T.unpack fieldName, value)

parsePassport :: Parser Passport
parsePassport = do
  many (parseField <* optional (void space1 <|> void newline)) <* eof

parseAllPassports :: String -> [Passport]
parseAllPassports input =
  mapMaybe (rightToMaybe . runParser parsePassport "input") passports
  where
    passports = T.splitOn (T.pack "\n\n") (T.pack input)

valid :: Passport -> Bool
valid =
  (==) 7 . length . filter ((/=) "cid") . map fst

part1 :: String -> Int
part1 = length . filter valid . parseAllPassports

parseInt :: T.Text -> Integer
parseInt = read . T.unpack

validField :: PassportField -> Bool
validField ("ecl", color) = (T.unpack color) `elem`  ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField ("pid", num) = T.length num == 9 && T.all isDigit num
validField ("cid", _) = True
validField ("byr", year) = parseInt year `elem` [1920..2002]
validField ("iyr", year) = parseInt year `elem` [2010..2020]
validField ("eyr", year) = parseInt year `elem` [2020..2030]
validField ("hcl", color) =
  if T.head color /= '#' then False
  else T.all (\ c -> c `elem` "abcdef1234567890") (T.tail color)
validField ("hgt", height) =
  if T.takeEnd 2 height == T.pack "in"
  then (parseInt $ T.dropEnd 2 height) `elem` [59..76]
  else if T.takeEnd 2 height == T.pack "cm"
  then (parseInt $ T.dropEnd 2 height) `elem` [150..193]
  else False
validField _ = False

part2 :: String -> Int
part2 = length . filter (all validField) . filter valid . parseAllPassports