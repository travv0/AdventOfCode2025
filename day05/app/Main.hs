module Main where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Range = Range Int Int deriving (Show, Eq)

instance Ord Range where
  compare (Range s1 e1) (Range s2 e2) = compare s1 s2 <> compare e1 e2

main :: IO ()
main = do
  (ranges, ids) <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (countFreshIngredients ranges ids)
  putStrLn $ "Part 2: " <> show (countAllIngredients ranges)

isFresh :: [Range] -> Int -> Bool
isFresh rs i = any (\(Range start end) -> start <= i && i <= end) rs

countFreshIngredients :: [Range] -> [Int] -> Int
countFreshIngredients rs = length . filter (isFresh rs)

parseInput :: String -> ([Range], [Int])
parseInput s = (mapMaybe parseRange ranges, mapMaybe readMaybe ids)
  where (rangeStr, idStr) = case splitOn "\n\n" s of
                              [r, i] -> (r, i)
                              _ -> error $ "bad input: " <> s
        ranges = lines rangeStr
        ids = lines idStr

parseRange :: String -> Maybe Range
parseRange s =
  case splitOn "-" s of
    [start, end] -> Range <$> readMaybe start <*> readMaybe end
    _ -> Nothing

mergeRanges :: Range -> Range -> [Range]
mergeRanges r1@(Range s1 e1) r2@(Range s2 e2) =
  if rangesIntersect r1 r2
     then [Range (min s1 s2) (max e1 e2)]
     else [r1, r2]

rangesIntersect :: Range -> Range -> Bool
rangesIntersect (Range _ e1) (Range s2 _) = e1 + 1 >= s2

mergeAllRanges :: [Range] -> [Range]
mergeAllRanges = go . sort
  where go (r1 : r2 : t) = case mergeRanges r1 r2 of
                             [mr1, mr2] -> mr1 : mergeAllRanges (mr2 : t)
                             [mr] -> mergeAllRanges $ mr : t
                             o -> error $ "bad mergeRanges output: " <> show o
        go rs = rs

countAllIngredients :: [Range] -> Int
countAllIngredients = sum . map (\(Range s e) -> e - s + 1) . mergeAllRanges
