{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Range = Range Int Int deriving (Show)

instance Eq Range where
  Range s1 e1 == Range s2 e2 = s1 == s2 && e1 == e2

instance Ord Range where
  compare (Range s1 e1) (Range s2 e2) = compare s1 s2 <> compare e1 e2

main :: IO ()
main = do
  (ranges, ids) <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (countFreshIngredients ranges ids)

isFresh :: [Range] -> Int -> Bool
isFresh rs i = any (\(Range start end) -> start <= i && i <= end) rs

countFreshIngredients :: [Range] -> [Int] -> Int
countFreshIngredients rs = length . filter (isFresh rs)

parseInput :: String -> ([Range], [Int])
parseInput s = (mapMaybe parseRange ranges, mapMaybe readMaybe ids)
  where [rangeStr, idStr] = splitOn "\n\n" s
        ranges = lines rangeStr
        ids = lines idStr

parseRange :: String -> Maybe Range
parseRange s =
  case splitOn "-" s of
    [start, end] -> Just $ Range (read start) (read end)
    _ -> Nothing

mergeRanges :: Range -> Range -> [Range]
mergeRanges r1@(Range s1 e1) r2@(Range s2 e2) =
  if rangesIntersect r1 r2
     then [Range (min s1 s2) (max e1 e2)]
     else [r1, r2]

rangesIntersect :: Range -> Range -> Bool
rangesIntersect (Range s1 e1) (Range s2 e2) =
  s2 <= s1 && s1 <= e2
  || s2 <= e1 && e1 <= e2
  || s1 <= s2 && s2 <= e1
  || s1 <= e2 && e2 <= e1

testInput = "3-5\n" ++
            "10-14\n" ++
            "16-20\n" ++
            "12-18\n" ++
            "\n" ++
            "1\n" ++
            "5\n" ++
            "8\n" ++
            "11\n" ++
            "17\n" ++
            "32"
