module Main where

import qualified Data.Map as Map
import Data.Maybe

type Part = [String] -> Int
type Rule = (String, [(String, Int)])

part1 :: Part
part1 x = length . filter id . map containsEventually $ (Map.keys m)
    where l = map parseRule x
          m = Map.fromList l
          containsEventually :: String -> Bool
          containsEventually x = case Map.lookup x m of
            Nothing -> False
            Just ys -> if elem "shinygold" (map fst ys) 
                        then True
                        else or . map (containsEventually . fst) $ ys

parseRule :: String -> Rule
parseRule = go1 "" . words
    where go1 s ("bags":"contain":xs) = (s, go2 "" [] xs)
          go1 s (x:xs) = go1 (s++x) xs
          go1 s []     = (s, [])
          go2 _ t ("no":"other":xs) = t
          go2 s t ("bags,":xs) = go2 "" (sep s:t) xs
          go2 s t ("bag,":xs)  = go2 "" (sep s:t) xs
          go2 s t ("bag.":xs)  = go2 "" (sep s:t) xs
          go2 s t ("bags.":xs) = go2 "" (sep s:t) xs
          go2 s t (x:xs)       = go2 (s++x) t xs
          go2 s t []           = t
          sep x = (tail x, read [head x])

part2 :: Part
part2 x = go "shinygold"
    where go k = fromMaybe 0 $ getBags <$> Map.lookup k  m
          l = map parseRule x
          m = Map.fromList l
          getBags ((k, v):ks) = v + getBags ks + v * go k
          getBags [] = 0

readInput :: IO ([String])
readInput = lines <$> readFile "input"

run :: (Part) -> [String] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
