module Day5 (
  part1,
  part2,
  parseRow,
  parseChair,
  seat
) where

import Data.Set(Set, fromList, member)

partition :: (Integer, Integer) -> Char -> (Integer, Integer)
partition (low, high) c
  | c `elem` ['F', 'L'] = (low, low + mid)
  | c `elem` ['B', 'R'] = (low + mid + 1, high)
  | otherwise = error ("Unexpected character in ticket " ++ show c)
  where
    mid = (high - low) `div` 2

parseRow :: String -> Integer
parseRow = uncurry min . foldl partition (0, 127) . take 7

parseChair :: String -> Integer
parseChair = uncurry min . foldl partition (0, 7) . drop 7

seat :: String -> Integer
seat ticket =
  row * 8 + chair
  where
    row = parseRow ticket
    chair = parseChair ticket

part1 :: [String] -> Integer
part1 = maximum . map seat

seats :: [String] -> [Integer]
seats = map seat

-- Your seat wasn't at the very front or back, though;
-- the seats with IDs +1 and -1 from yours will be in your list.
-- What is the ID of your seat?
part2 :: [String] -> Integer
part2 tickets =
   (+) 1 $ head $ filter isMySeat allSeats
  where
    allSeats = seats tickets
    seatsSet = fromList allSeats
    isMySeat seatId =
      member (seatId+2) seatsSet && not (member (seatId+1) seatsSet)

