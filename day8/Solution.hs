{-# LANGUAGE TypeApplications #-}

module Main where

import Data.IntMap (IntMap, (!?), insert, fromAscList, toAscList)
import Data.List (intercalate, intersperse)

type Part = [String] -> Computation

data Op = Nop Int | Jmp Int | Acc Int deriving Show
data State  = Running | Looping | Terminated deriving Show
type Program = IntMap (Op, Int)
data Computation = Λ State Int Program
instance Show Computation where
    show (Λ t acc prog) = "Λ " ++ show t ++ ": " ++ show acc

compile :: [String] -> Program
compile = fromAscList . zip [1..] . map (flip (,) 0 . cmp)
    where cmp ('a':'c':'c':xs) = Acc (arg . tail $ xs)
          cmp ('j':'m':'p':xs) = Jmp (arg . tail $ xs)
          cmp ('n':'o':'p':xs) = Nop (arg . tail $ xs)
          cmp _                = Nop 0
          arg ('+':xs) =          read @Int xs
          arg ('-':xs) = negate . read @Int $ xs
          arg _        = 0

load :: Program -> Computation
load = Λ Running 0

compute :: Computation -> Computation
compute = go 1
    where go :: Int -> Computation -> Computation
          go p (Λ t acc prog) = case (prog!?p) of
            Just (Nop a, 0) -> go (p+1) (Λ Running acc     $ insert p (Nop a, p) prog)
            Just (Acc a, 0) -> go (p+1) (Λ Running (acc+a) $ insert p (Acc a, p) prog)
            Just (Jmp a, 0) -> go (p+a) (Λ Running acc     $ insert p (Jmp a, p) prog)
            Just _          -> (Λ Looping    acc prog)
            Nothing         -> (Λ Terminated acc prog)

part1 :: Part
part1 = compute . load . compile

fixProgram :: Program -> [Program]
fixProgram = go 1
    where go :: Int -> Program -> [Program]
          go p prog = case (prog!?p) of
            Just (Nop a, 0) -> insert p (Jmp a, 0) prog : go (p+1) prog
            Just (Acc a, 0) -> go (p+1) (insert p (Acc a, 0) prog)
            Just (Jmp a, 0) -> insert p (Nop a, 0) prog : go (p+1) prog
            Nothing         -> [prog]

xx = map (compute . load) . fixProgram . compile
    where terminates :: Computation -> Bool
          terminates (Λ Terminated acc p) = True
          terminates _                    = False

part2 :: Part
part2 = head . filter terminates . map (compute . load) . fixProgram . compile
    where terminates :: Computation -> Bool
          terminates (Λ Terminated acc p) = True
          terminates _                    = False

readInput :: IO ([String])
readInput = lines <$> readFile "input"

run :: Part -> [String] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
