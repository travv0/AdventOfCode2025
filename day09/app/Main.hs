module Main where

import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)

data Tile = Tile Int Int deriving (Eq, Ord, Show)

main :: IO ()
main = do
  tiles <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (maximum $ calcAreas tiles)

parseInput :: String -> [Tile]
parseInput = mapMaybe parseLine . lines

parseLine :: String -> Maybe Tile
parseLine s = case splitOn "," s of
                [x, y] -> Just $ Tile (read x) (read y)
                _ -> Nothing

calcAreas :: [Tile] -> [Int]
calcAreas tiles = [area | tile1@(Tile x1 y1) <- tiles
                        , tile2@(Tile x2 y2) <- tiles
                        , let area = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
                        , tile1 < tile2]
