module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (sumJoltages 2 input)
  putStrLn $ "Part 2: " <> show (sumJoltages 12 input)

findLargestDigit :: String -> (Char, Int)
findLargestDigit s = maximumBy (comparing fst) $ reverse $ zip s [0 ..]

findLargestJoltage :: Int -> String -> String
findLargestJoltage 0 _ = ""
findLargestJoltage len s =
  digit : findLargestJoltage (len - 1) (drop (i + 1) s)
    where (digit, i) = findLargestDigit $ take (length s - (len - 1)) s

sumJoltages :: Int -> [String] -> Int
sumJoltages len = sum . map (read . findLargestJoltage len)
