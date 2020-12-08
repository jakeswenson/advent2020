{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Day7 (
  part1,
  part2,
  parseBagName,
  parseEdge,
  parseBagSpec,
  parseLine,
  BagName(..),
  BagEdge(..),
  Bag(..),
  bagEdge,
  parseEdges
) where


import Control.Monad
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum(..))
import Data.Either(fromRight)
import Data.Either.Combinators (rightToMaybe)
import qualified Data.Text as T
import Data.Text (splitOn, count, Text)
import Text.Megaparsec
import Text.Megaparsec.Char (char, lowerChar, space, space1, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void
import Data.Char(isAlpha)
-- import Algebra.Graph.AdjacencyMap(edges, hasVertex, overlay)
import Data.Graph.Wrapper(Graph, fromList, reachableVertices, transpose, fromListLenient, fromListSimple, vertex)

data BagName = BagName Text | Other
  deriving (Show, Eq, Ord)

data BagEdge = BagEdge BagName Int BagName
  deriving (Show, Eq, Ord)

data Bag = Bag BagName [BagEdge]
  deriving (Show, Eq, Ord)

type Parser = Parsec Void Text

-- bright white
-- vibrant plum
-- faded blue
parseBagName :: Parser BagName
parseBagName = do
  texture <- takeWhile1P (Just "bagTexture") isAlpha
  space1
  color <- takeWhile1P (Just "bagColor") isAlpha
  return $ BagName $ T.unwords [texture, color]

parseOtherBag :: Parser BagName
parseOtherBag = do
  string "other"
  return Other

-- 1
-- 5
-- no
parseCount :: Parser Int
parseCount = decimal <|> ((\_ -> 0) <$> string "no")

-- 1 shiny gold bag.
-- 5 faded blue bags
-- 6 dotted black bags.
-- no other bags.
bagEdge :: Parser (Int, BagName)
bagEdge = do
  count <- parseCount
  space1
  name <- (try parseOtherBag <|> parseBagName)
  space1
  string "bag"
  optional (char 's')
  return $ (count, name)

-- 1 shiny gold bag.
-- 5 faded blue bags
-- 6 dotted black bags.
-- no other bags.
parseEdge :: BagName -> Parser BagEdge
parseEdge parent = do
  (count, name) <- bagEdge
  return $ BagEdge parent count name

-- 1 shiny gold bag.
-- 5 faded blue bags, 6 dotted black bags.
-- no other bags.
parseEdges :: BagName -> Parser [BagEdge]
parseEdges parent =
  (space *> parseEdge parent) `sepBy` (char ',' <* space)

-- bright white bags contain 1 shiny gold bag.
-- vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
-- faded blue bags contain no other bags.
parseBagSpec :: Parser Bag
parseBagSpec = do
  bagName <- parseBagName
  space1
  string "bags"
  space1
  string "contain"
  space1
  edges <- parseEdges bagName
  char '.'
  return $ Bag bagName edges

parseLine :: String -> Bag
parseLine line =
  fromRight (error ("Couldn't parse line: " ++ (show line))) $ runParser parseBagSpec "line" $ T.pack line

transitiveClosure :: BagName -> Graph BagName Bag -> [BagName]
transitiveClosure node = (flip reachableVertices) node

buildInvertedGraph :: [Bag] -> Graph BagName Bag
buildInvertedGraph = transpose . buildGraph

buildGraph :: [Bag] -> Graph BagName Bag
buildGraph bags = fromListLenient $ do
  bag@(Bag source bagEdges) <- bags
  return (source, bag, [target | (BagEdge _ _ target) <- bagEdges])

part1 :: String -> Int
part1 =
  findWays (BagName "shiny gold") . map parseLine . lines
  where
    findWays :: BagName -> [Bag] -> Int
    findWays target =
      -- re:`pred` - have to subtract one for some reason...
      -- not sure why (maybe the starting node is included with reachableVertices)
      pred . length . transitiveClosure target . buildInvertedGraph

totalBagsCount :: BagName -> Graph BagName Bag -> Int
totalBagsCount bag graph =
  bagSize $ lookupBag bag
  where

    lookupBag :: BagName -> [BagEdge]
    lookupBag = bagEdges . vertex graph

    bagEdges :: Bag -> [BagEdge]
    bagEdges (Bag _ edges) = edges

    bagSize [] = 0
    bagSize [(BagEdge _ 0 _)] = 0
    bagSize ((BagEdge _ count target):rest) =
      count + count * bagSize (lookupBag target)  + (bagSize rest)


part2 :: String -> Int
part2 =
  totalBagsCount (BagName "shiny gold") . buildGraph . map parseLine . lines
