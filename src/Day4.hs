{-# language OverloadedStrings #-}
module Day4 (
  Passport(..),
  part1,
  part2,
  parseField,
  parsePassport,
  parseAllPassports
) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Char (digitToInt, isAlpha, isSpace, isDigit)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (fold, foldl')
import Data.Maybe (mapMaybe)
import Data.Monoid (Alt(..))
import qualified Data.Text as T
import Data.Text (splitOn, count, Text)
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, hexDigitChar, newline, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

-- byr (Birth Year)
   --iyr (Issue Year)
   --eyr (Expiration Year)
   --hgt (Height)
   --hcl (Hair Color)
   --ecl (Eye Color)
   --pid (Passport ID)
   --cid (Country ID)
data Passport = Passport
  { byr :: Maybe Integer
  , iyr :: Maybe Integer
  , eyr :: Maybe Integer
  , hgt :: Maybe Integer -- in cm
  , hcl :: Maybe Text
  , ecl :: Maybe Text
  , pid :: Maybe Integer
  , cid :: Maybe Integer
  } deriving (Eq, Show)

instance Semigroup Passport where
  x <> y = Passport
    { byr = byr x <|> byr y
    , iyr = iyr x <|> iyr y
    , eyr = eyr x <|> eyr y
    , hgt = hgt x <|> hgt y
    , hcl = hcl x <|> hcl y
    , ecl = ecl x <|> ecl y
    , pid = pid x <|> pid y
    , cid = cid x <|> cid y
    }

instance Monoid Passport where
  mempty = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

decimalInRange :: Integer -> Integer -> Parser Integer
decimalInRange min max = do
  x <- decimal
  guard $ x >= min && x <= max
  return x

parsePid :: Parser Passport
parsePid = do
  _ <- string "pid:"
  digits <- replicateM 9 digitChar
  return $! mempty { pid = Just . toInteger $ foldl' (\ x y -> x * 10 + digitToInt y) 0 digits }

parseHgt :: Parser Passport
parseHgt = do
  _ <- string "hgt:"
  let hgt_in = decimalInRange 59 76 <* string "in"
  let hgt_cm = decimalInRange 150 193 <* string "cm"
  hgt <- try hgt_cm <|> try hgt_in
  return $! mempty { hgt = Just hgt }

parseHcl :: Parser Passport
parseHcl = do
  _ <- string "hcl:#"
  color <- replicateM 6 hexDigitChar
  return $! mempty { hcl = Just $ T.pack color }

parseByr :: Parser Passport
parseByr = do
  year <- string "byr:" *> decimalInRange 1920 2002
  return $ mempty { byr = Just year }

parseIyr :: Parser Passport
parseIyr = do
  year <- string "iyr:" *> decimalInRange 2010 2020
  return $ mempty { iyr = Just year }

parseEyr :: Parser Passport
parseEyr = do
  year <- string "eyr:" *> decimalInRange 2020 2030
  return $ mempty { eyr = Just year }

parseEcl :: Parser Passport
parseEcl = do
  _ <- string "ecl:"
  color <- getAlt . foldMap (Alt . string) $ ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  return $ mempty { ecl = Just color }

parseCid :: Parser Passport
parseCid = do
  value <- string "cid:" *> decimal
  return $ mempty { cid = Just value }

parseField :: Parser Passport
parseField = parseByr <|> parseIyr <|> parseEyr <|> parseHgt <|> parseHcl <|> parseEcl <|> parsePid <|> parseCid

parsePassport :: Parser Passport
parsePassport = do
  fields <- trace "fields "$ many (parseField <* optional (void space1 <|> void newline)) <* eof
  return $ fold fields

parseAllPassports :: String -> [Passport]
parseAllPassports input =
  mapMaybe (rightToMaybe . runParser parsePassport "input") passports
  where
    passports = T.splitOn (T.pack "\n\n") (T.pack input)

part1 :: String -> Int
part1 = sum . map validFields . parseAllPassports

validFields :: Passport -> Int
validFields _ = error "foo"

validPassport :: Passport -> Bool
validPassport Passport
  { byr = Just _
  , iyr = Just _
  , eyr = Just _
  , hgt = Just _
  , hcl = Just _
  , ecl = Just _
  , pid = Just _
  , cid = _ } = True
validPassport _ = False

part2 :: String -> Int
part2 = length . filter validPassport . parseAllPassports