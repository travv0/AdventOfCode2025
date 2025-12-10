module Main where

import Data.Maybe (mapMaybe)
import Data.List (sortOn, findIndex)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Read (readMaybe)
import Data.Ord (Down (Down))

data Box = Box Int Int Int deriving (Eq, Ord, Show)
type Circuit = Set Box

main :: IO ()
main = do
  boxes <- parseInput <$> readFile "input.txt"
  let (circuits, _) = buildCircuits boxes 1000
  putStrLn $
    "Part 1: " <>
      show (product $ take 3 $ sortOn Down (S.size <$> circuits))
  let (_, finalProduct) = buildCircuits boxes (maxBound :: Int)
  putStrLn $
    "Part 2: " <> maybe "Failed to find final pair" show finalProduct

calcDistances :: [Box] -> [(Box, Box, Double)]
calcDistances boxes = [(box1, box2, dist) | box1 <- boxes
                                          , box2 <- boxes
                                          , let dist = calcDistance box1 box2
                                          , box1 < box2]

calcDistance :: Box -> Box -> Double
calcDistance (Box x1 y1 z1) (Box x2 y2 z2) =
  sqrt $ (fromIntegral x1 - fromIntegral x2) ** 2
       + (fromIntegral y1 - fromIntegral y2) ** 2
       + (fromIntegral z1 - fromIntegral z2) ** 2

buildCircuits :: [Box] -> Int -> ([Circuit], Maybe Int)
buildCircuits boxes =
  go [] (sortOn (\(_, _, d) -> d) $ calcDistances boxes) Nothing
  where
    go :: [Circuit]
       -> [(Box, Box, Double)]
       -> Maybe Int
       -> Int
       -> ([Circuit], Maybe Int)
    go circuits _ finalProduct 0 = (circuits, finalProduct)
    go circuits
       ((box1@(Box x1 _ _), box2@(Box x2 _ _), _) : distances)
       finalProduct
       n =
      case (findIndex (S.member box1) circuits,
            findIndex (S.member box2) circuits) of
        (Just i1, Just i2) ->
          if i1 == i2
             then go circuits distances finalProduct (n - 1)
             else let mergedCircuits = (mergeCircuits i1 i2 circuits)
                   in if length mergedCircuits == 1
                         then go mergedCircuits distances (Just (x1 * x2)) 0
                         else go mergedCircuits distances finalProduct (n - 1)
        (Just i1, Nothing) ->
          go (updateCircuit i1 box2 circuits)
             distances
             finalProduct
             (n - 1)
        (Nothing, Just i2) ->
          go (updateCircuit i2 box1 circuits)
             distances
             finalProduct
             (n - 1)
        _ -> go (S.fromList [box1, box2] : circuits)
                distances
                finalProduct
                (n - 1)
    go circuits _ _ _ = error $ show circuits


updateCircuit :: Int -> Box -> [Circuit] -> [Circuit]
updateCircuit i box circuits =
  case splitAt i circuits of
    (headCircuits, circuit : tailCircuits) ->
      headCircuits ++ (S.insert box circuit : tailCircuits)
    _ -> circuits

mergeCircuits :: Int -> Int -> [Circuit] -> [Circuit]
mergeCircuits i1 i2 circuits =
  case splitAt (min i1 i2) circuits of
    (headCircuits, circuit1 : restCircuits) ->
      case splitAt (abs (i1 - i2) - 1) restCircuits of
        (midCircuits, circuit2 : tailCircuits) ->
          headCircuits
            ++ midCircuits
            ++ (S.union circuit1 circuit2 : tailCircuits)
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
