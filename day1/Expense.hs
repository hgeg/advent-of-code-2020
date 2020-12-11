module Main where

import Data.Maybe


findPairs :: [Int] -> [Int]
findPairs xs = map multiplied . (filter bySum) $ allPairs xs
    where multiplied (a, b)    = a * b
          bySum (a, b)    = a+b == 2020
          allPairs xs = [(a, b) | a <- xs, b <- xs]

findTriads :: [Int] -> [Int]
findTriads xs = map multiplied . (filter bySum) $ allPairs xs
    where multiplied (a, b, c) = a * b * c
          bySum (a, b, c) = a+b+c == 2020
          allPairs xs = [(a, b, c) | a <- xs, b <- xs, c <- xs]

main :: IO ()
main = readFile "input" >>= (putStrLn . show . findTriads . map read . lines)
