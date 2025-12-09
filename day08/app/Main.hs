module Main where

import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Read (readMaybe)

data Box = Box Double Double Double deriving (Eq, Show)
type Circuit = Set Box

main :: IO ()
main = putStrLn "Hello, Haskell!"

calcDistances :: [Box] -> [(Box, Box, Double)]
calcDistances boxes = [(box1, box2, dist) | box1 <- boxes
                                          , box2 <- boxes
                                          , let dist = calcDistance box1 box2
                                          , box1 /= box2]

calcDistance :: Box -> Box -> Double
calcDistance (Box x1 y1 z1) (Box x2 y2 z2) =
  sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2

parseInput :: String -> [Box]
parseInput = catMaybes . map parseLine . lines

parseLine :: String -> Maybe Box
parseLine s = case splitOn "," s of
                [xs, ys, zs] -> do
                  x <- readMaybe xs
                  y <- readMaybe ys
                  z <- readMaybe zs
                  Just $ Box x y z
                _ -> Nothing

testBoxes :: [Box]
testBoxes =
  [ Box 162 817 812
  , Box 57 618 57
  , Box 906 360 560
  , Box 592 479 940
  , Box 352 342 300
  , Box 466 668 158
  , Box 542 29 236
  , Box 431 825 988
  , Box 739 650 466
  , Box 52 470 668
  , Box 216 146 977
  , Box 819 987 18
  , Box 117 168 530
  , Box 805 96 715
  , Box 346 949 466
  , Box 970 615 88
  , Box 941 993 340
  , Box 862 61 35
  , Box 984 92 344
  , Box 425 690 689 ]
