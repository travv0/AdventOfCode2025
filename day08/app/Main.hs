module Main where

import Data.Maybe (mapMaybe)
import Data.List (sortOn, findIndex, sortBy)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Read (readMaybe)
import Data.Ord (comparing, Down (Down))

data Box = Box Double Double Double deriving (Eq, Ord, Show)
type Circuit = Set Box

main :: IO ()
main = do
  boxes <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (product $ take 3 $ sortBy (comparing Down) (S.size <$> buildCircuits boxes 1000))

calcDistances :: [Box] -> [(Box, Box, Double)]
calcDistances boxes = [(box1, box2, dist) | box1 <- boxes
                                          , box2 <- boxes
                                          , let dist = calcDistance box1 box2
                                          , box1 < box2]

calcDistance :: Box -> Box -> Double
calcDistance (Box x1 y1 z1) (Box x2 y2 z2) =
  sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 + (z1 - z2) ** 2

buildCircuits :: [Box] -> Int -> [Circuit]
buildCircuits boxes = go [] (sortOn (\(_, _, d) -> d) $ calcDistances boxes)
  where
    go :: [Circuit] -> [(Box, Box, Double)] -> Int -> [Circuit]
    go circuits _ 0 = circuits
    go circuits ((box1, box2, _) : distances) n =
      case (findIndex (S.member box1) circuits, findIndex (S.member box2) circuits) of
        (Just i1, Just i2) ->
          if i1 == i2
             then go circuits distances (n - 1)
             else go (mergeCircuits i1 i2 circuits) distances (n - 1)
        (Just i1, Nothing) ->
          go (updateCircuit i1 box1 box2 circuits) distances (n - 1)
        (Nothing, Just i2) ->
          go (updateCircuit i2 box1 box2 circuits) distances (n - 1)
        _ -> go (S.fromList [box1, box2] : circuits) distances (n - 1)
    go circuits _ _ = error $ show circuits


updateCircuit :: Int -> Box -> Box -> [Circuit] -> [Circuit]
updateCircuit i box1 box2 circuits =
  case splitAt i circuits of
    (headCircuits, circuit : tailCircuits) ->
      headCircuits ++ (S.insert box1 (S.insert box2 circuit) : tailCircuits)
    _ -> circuits

mergeCircuits :: Int -> Int -> [Circuit] -> [Circuit]
mergeCircuits i1 i2 circuits =
  case splitAt (min i1 i2) circuits of
    (headCircuits, circuit1 : restCircuits) ->
      case splitAt (abs (i1 - i2) - 1) restCircuits of
        (midCircuits, circuit2 : tailCircuits) ->
          headCircuits ++ midCircuits ++ (S.union circuit1 circuit2 : tailCircuits)
        a -> error $ "Bad mergeCircuits 2: " <> show a
    a -> error $ "Bad mergeCircuits 1: " <> show a

parseInput :: String -> [Box]
parseInput = mapMaybe parseLine . lines

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
