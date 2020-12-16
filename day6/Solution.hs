module Main where

import Data.Set (fromList, size, intersection, elems)

type Part = [String] -> Int

groupWith :: (String -> String -> String) -> [String] -> [String]
groupWith f (x:"":xs) = x : groupWith f xs
groupWith f (x:y:ys)  = groupWith f $ (f x y) : ys
groupWith _ [x]       = [x]

part1 :: Part
part1 = sum . map (size . fromList) . groupWith (++)

part2 :: Part
part2 = sum . map (size . fromList) . groupWith yesAll
    where yesAll x y = elems (fromList x `intersection` fromList y)

readInput :: IO ([String])
readInput = lines <$> readFile "input"

run :: (Part) -> [String] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
