module Main where

import Data.Semigroup
import Data.Set (fromList, member, elems, (\\))

type Part = [String] -> Int

part1 :: Part
part1 = getMax . mconcat . map (Max . seatID . getSeat)

seatID :: (Int, Int) -> Int
seatID (r, c) = r * 8 + c

getSeat :: String -> (Int, Int)
getSeat = go (0, 0)
    where go :: (Int, Int) -> String -> (Int, Int)
          go (a,b) (x:xs) = case x of
            'B'       -> go (a + 2^(length xs - 3), b) xs
            'R'       -> go (a, b + 2^(length xs)) xs
            otherwise -> go (a, b) xs
          go p [] = p

part2 :: Part
part2 xs = head $ filter hasNeighbors $ elems empty
    where taken = fromList $ map (seatID . getSeat) xs
          empty = fromList [0..1032] \\ taken
          hasNeighbors x = member (x-1) taken && member (x+1) taken

readInput :: IO ([String])
readInput = lines <$> readFile "input"

run :: (Part) -> [String] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
