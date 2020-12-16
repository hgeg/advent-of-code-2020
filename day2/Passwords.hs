module Main where

data Password = Password (Int, Int) Char String deriving Show

password :: String -> Password
password x = Password (min, max) c v
    where min = read . takeWhile (/='-') $ x :: Int
          max = read . tail . dropWhile (/='-') . takeWhile (/=' ') $ x :: Int
          c   = head . tail . dropWhile (/=' ') . takeWhile (/=':') $ x :: Char
          v   = tail . tail . dropWhile (/=':') $ x :: String

validateOld :: Password -> Bool
validateOld (Password (min, max) c v) = count >= min && count <= max
    where count = length . filter (==c) $ v

validateNew :: Password -> Bool
validateNew (Password (min, max) c v) = (c1 == c && c2 /= c) || (c1 /= c && c2 == c)
    where c1 = head . drop (min-1) $ v
          c2 = head . drop (max-1) $ v

readInput :: IO ([String])
readInput = lines <$> readFile "input"

part1 :: [String] -> Int
part1 = length . filter validateOld . map password

part2 :: [String] -> Int
part2 = length . filter validateNew . map password

main :: IO ()
main = readInput >>= (putStrLn . show . part2)

