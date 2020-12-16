module Main where

type Part = [String] -> *

part1 :: Part
part1 = undefined

part2 :: Part
part2 = undefined

readInput :: IO ([String])
readInput = lines <$> readFile "input"

run :: (Part) -> [String] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
