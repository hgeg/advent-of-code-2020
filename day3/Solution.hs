module Main where

type Part = [String] -> Int

part1 :: Part
part1 = move 0
    where checkTree n r = if (head . drop (n `rem` length r) $ r) == '#' then 1 else 0
          move n (x:y:ys) = move (n+3) (y:ys) + checkTree (n+3) y
          move n [x] = checkTree (n+3) x

part2 :: Part
part2 m = product . map (move 0 m) $ slopes
    where slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
          checkTree n [] = 0
          checkTree n r  = if (head . drop (n `rem` length r) $ r) == '#' then 1 else 0
          pop [] = []
          pop m = head m
          move :: Int -> [String] -> (Int, Int) -> Int
          move n [] _ = 0
          move n m (r, d) = move (n+1) (drop d m) (r, d) + checkTree (n*r+r) (pop $ drop d m)

readInput :: IO ([String])
readInput = lines <$> readFile "input"

run :: (Part) -> [String] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
