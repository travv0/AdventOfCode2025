module Main where

import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  insts <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (countZeros landsOnZero 0 50 insts)
  putStrLn $ "Part 2: " <> show (countZeros pointsAtZero 0 50 insts)

data Dir = L | R deriving (Show, Read, Eq)

countZeros :: (Int -> Dir -> Int -> Int) -> Int -> Int -> [(Dir, Int)] -> Int
countZeros countFn zeros curr ((dir, n):insts) =
  countZeros countFn (zeros + newZeros) (shift curr dir n) insts
    where newZeros = countFn curr dir n
countZeros _ zeros _ [] = zeros

landsOnZero :: Int -> Dir -> Int -> Int
landsOnZero curr dir n = if shift curr dir n == 0 then 1 else 0

pointsAtZero :: Int -> Dir -> Int -> Int
pointsAtZero curr L n = (100 - curr' + n) `div` 100
  where curr' = if curr == 0 then 100 else curr
pointsAtZero curr R n = (curr + n) `div` 100

shift :: Int -> Dir -> Int -> Int
shift curr dir n = curr `op` n `mod` 100
  where op = case dir of
               L -> (-)
               R -> (+)

parseInput :: String -> [(Dir, Int)]
parseInput = mapMaybe parseLine . lines

parseLine :: String -> Maybe (Dir, Int)
parseLine (c:n) = Just (read [c], read n)
parseLine _ = Nothing
