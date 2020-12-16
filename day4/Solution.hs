module Main where

import qualified Data.Map as M
import Data.Map ((!?), (!))
import Data.Maybe (maybe)
import Text.Read (readMaybe)
import Data.List
import Data.Char

import qualified Text.Parsec as Parsec
import Control.Applicative
import Data.Either

type Part = [String] -> Int
type Passport = M.Map String String

-- association operator as a syntactic sugar
infixl 0 =:
(=:) :: a -> b -> (a, b)
(=:) = (,)

split :: [String] -> [String]
split (x:"":xs) = x : split xs
split (x:y:ys)  = split $ (x++" "++y) : ys
split [x]       = [x]

mkPass :: String -> Passport
mkPass = M.fromList . map (\x->(take 3 x, drop 4 x)) . words

validate :: Passport -> Bool
validate p = and $ map (\x -> M.member x p) reqFields
    where reqFields = ["byr", "iyr", "eyr", "hgt", "ecl", "hcl", "pid"]

validateFields :: Passport -> Bool
validateFields p = and $ maybe False id <$> check <$> reqFields
    where reqFields = ["byr", "iyr", "eyr", "hgt", "ecl", "hcl", "pid"]
          check :: String -> Maybe Bool
          check x = (p !? x) >>= (fieldChecks ! x)
          fieldChecks :: M.Map String (String -> Maybe Bool)
          fieldChecks = M.fromList [
            "byr" =: fmap (\x -> x >= 1920 && x <= 2002) . readMaybe ,
            "iyr" =: fmap (\x -> x >= 2010 && x <= 2020) . readMaybe ,
            "eyr" =: fmap (\x -> x >= 2020 && x <= 2030) . readMaybe ,
            "hgt" =: Just . validateHeight ,
            "hcl" =: Just . validateHairColor ,
            "ecl" =: Just . flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
            "pid" =: Just . validatePID ]

validateHeight :: String -> Bool
validateHeight h = fromRight False $ inRange <$> Parsec.parse rule "(source)" h
    where rule = Parsec.many1 Parsec.digit >>= (\x -> Parsec.string "cm" <|> Parsec.string "in" >>= (\y-> return (read x :: Int, y)))
          inRange (x, "in") = x>=59  && x<=76
          inRange (x, "cm") = x>=150 && x<=193

validateHairColor :: String -> Bool
validateHairColor c = isRight $ Parsec.parse rule "(source)" c
    where rule = Parsec.char '#' >> Parsec.count 6 (Parsec.oneOf "0123456789abcdef")

validatePID :: String -> Bool
validatePID pid = isRight $ Parsec.parse rule "(source)" pid
    where rule = Parsec.count 9 Parsec.digit

part1 :: Part
part1 = length . filter validate . map mkPass . split

part2 :: Part
part2 = length . filter validateFields . map mkPass . split

readInput :: IO ([String])
readInput = lines <$> readFile "input"

run :: (Part) -> [String] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
