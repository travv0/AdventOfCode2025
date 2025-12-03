module Main where

import Data.List.Split (splitOn, chunksOf)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  ranges <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (sumInvalidIds repeatsOnce ranges)
  putStrLn $ "Part 2: " <> show (sumInvalidIds repeatsMany ranges)

repeatsOnce :: Int -> Bool
repeatsOnce = uncurry (==) . splitHalf . show

repeatsMany :: Int -> Bool
repeatsMany n = any chunkRepeats chunks
  where s = show n
        chunks = filter ((> 1) . length) $ fmap (`chunksOf` s) [1 .. length s]

chunkRepeats :: [String] -> Bool
chunkRepeats (h:t) = all (== h) t
chunkRepeats _ = False

splitHalf :: String -> (String, String)
splitHalf s = splitAt (length s `div` 2) s

parseInput :: String -> [(Int, Int)]
parseInput = mapMaybe parseRange . splitOn ","

parseRange :: String -> Maybe (Int, Int)
parseRange s = case splitOn "-" s of
  [a, b] -> Just (read a, read b)
  _ -> Nothing

getInvalidIdsInRange :: (Int -> Bool) -> (Int, Int) -> [Int]
getInvalidIdsInRange invalidFn (a, b) =
  filter invalidFn [a .. b]

sumInvalidIds :: (Int -> Bool) -> [(Int, Int)] -> Int
sumInvalidIds invalidFn = sum . concatMap (getInvalidIdsInRange invalidFn)
