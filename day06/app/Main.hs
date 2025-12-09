module Main where

import Data.List (transpose)

data Problem = Addition [Int] | Multiplication [Int] deriving Show

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn $ "Part 1: " <> show (sumAnswers parseInput input)
  putStrLn $ "Part 2: " <> show (sumAnswers parseCephalopodInput input)

eval :: Problem -> Int
eval (Addition xs) = sum xs
eval (Multiplication xs) = product xs

sumAnswers :: (String -> [Problem]) -> String -> Int
sumAnswers parser = sum . fmap eval . parser

parseInput :: String -> [Problem]
parseInput = fmap (parseProblem . reverse) . transpose . fmap words . lines

parseProblem :: [String] -> Problem
parseProblem ("+" : nums) = Addition $ fmap read nums
parseProblem ("*" : nums) = Multiplication $ fmap read nums
parseProblem ls = error $ "Bad parseProblem input: " <> show ls

parseCephalopodInput :: String -> [Problem]
parseCephalopodInput = parseCephalopodLines . lines

parseCephalopodLines :: [String] -> [Problem]
parseCephalopodLines ("" : _) = []
parseCephalopodLines ls = problem : parseCephalopodLines rests
  where widest = maximum $ fmap (length . head . words) ls
        splitLines = fmap (splitAt widest) ls
        col = fmap fst splitLines
        rests = fmap (drop 1 . snd) splitLines
        nums = fmap read $ transpose $ init col
        problem = case filter (/= ' ') $ last col of
                    "+" -> Addition nums
                    "*" -> Multiplication nums
                    s -> error $ "Bad op input: " <> show s
