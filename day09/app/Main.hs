module Main where

import Data.Maybe (mapMaybe, catMaybes)
import Data.List.Split (splitOn)

data Tile = Tile Double Double deriving (Eq, Ord, Show)
data Edge = Edge Tile Tile deriving (Show)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let tiles = parseTiles input
  let edges = parseEdges input
  let areas = calcAreas tiles
  print $ length areas
  let validAreas = calcValidAreas tiles edges
  print $ length validAreas
  putStrLn $ "Part 1: " <> (show :: Int -> String) (floor $ maximum $ calcAreas tiles)
  putStrLn $ "Part 2: " <> (show :: Int -> String) (floor $ maximum $ calcValidAreas tiles edges)

parseTiles :: String -> [Tile]
parseTiles = mapMaybe parseLine . lines

parseEdges :: String -> [Edge]
parseEdges s = catMaybes ((Edge <$> lastTile <*> firstTile) : go ls)
  where
    ls = lines s
    firstTile = parseLine $ head ls
    lastTile = parseLine $ last ls
    go (s1 : s2 : rest) = (Edge <$> parseLine s1 <*> parseLine s2) : go (s2 : rest)
    go _ = []

tileInPolygon :: Tile -> [Edge] -> Bool
tileInPolygon (Tile px py) = (== 1) . (`mod` 2) . length . filter intersectsEdge
  where xIntersect (Edge (Tile vx1 vy1) (Tile vx2 vy2)) =
          vx1 + (py - vy1) * (vx2 - vx1) / (vy2 - vy1)
        intersectsEdge edge@(Edge (Tile _ vy1) (Tile _ vy2)) =
          min vy1 vy2 < py && py < max vy1 vy2 && px < xIntersect edge

edgesIntersect :: Edge -> Edge -> Bool
edgesIntersect (Edge (Tile ax1 ay1) (Tile ax2 ay2))
               (Edge (Tile bx1 by1) (Tile bx2 by2))
  | ax1 == ax2 && bx1 == bx2 = False
  | ay1 == ay2 && by1 == by2 = False
  -- a is vertical, b is horizontal
  | ax1 == ax2 = min ay1 ay2 <= by1 && by1 <= max ay1 ay2
                 && min bx1 bx2 <= ax1 && ax1 <= max bx1 bx2
                 && ay1 /= by1 && ay2 /= by1
  -- a is horizontal, b is vertical
  | bx1 == bx2 = min by1 by2 <= ay1 && ay1 <= max by1 by2
                 && min ax1 ax2 <= bx1 && bx1 <= max ax1 ax2
                 && by1 /= ay1 && by2 /= ay1
  | otherwise = error "edges must be horizontal or vertical"

anyEdgesIntersect :: Edge -> [Edge] -> Bool
anyEdgesIntersect edge = any id . fmap (edgesIntersect edge)

rectangleInPolygon :: Tile -> Tile -> [Edge] -> Bool
rectangleInPolygon (Tile x1 y1) (Tile x2 y2) edges =
  all (flip tileInPolygon edges) sideTiles
    where
      sideTiles = concat [[Tile x y1 | x <- [min x1 x2 .. max x1 x2]],
                          [Tile x y2 | x <- [min x1 x2 .. max x1 x2]],
                          [Tile x1 y | y <- [min y1 y2 .. max y1 y2]],
                          [Tile x2 y | y <- [min y1 y2 .. max y1 y2]]]

parseLine :: String -> Maybe Tile
parseLine s = case splitOn "," s of
                [x, y] -> Just $ Tile (read x) (read y)
                _ -> Nothing

calcAreas :: [Tile] -> [Double]
calcAreas tiles = [area | tile1@(Tile x1 y1) <- tiles
                        , tile2@(Tile x2 y2) <- tiles
                        , let area = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
                        , tile1 < tile2]


calcValidAreas :: [Tile] -> [Edge] -> [Double]
calcValidAreas tiles edges =
  [area | tile1@(Tile x1 y1) <- tiles
        , tile2@(Tile x2 y2) <- tiles
        , let area = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
        , let res = rectEdges tile1 tile2
        , tile1 < tile2
        , not (any (flip anyEdgesIntersect res) edges)]
        -- , all (flip tileInPolygon edges) (rectCorners tile1 tile2)]
        -- , rectangleInPolygon tile1 tile2 edges]

rectEdges :: Tile -> Tile -> [Edge]
rectEdges (Tile x1 y1) (Tile x2 y2) =
  [ Edge (Tile x1 y1) (Tile x1 y2)
  , Edge (Tile x1 y1) (Tile x2 y1)
  , Edge (Tile x2 y2) (Tile x1 y2)
  , Edge (Tile x2 y2) (Tile x2 y1) ]

rectCorners :: Tile -> Tile -> [Tile]
rectCorners (Tile x1 y1) (Tile x2 y2) =
  [ Tile x1 y1
  , Tile x1 y2
  , Tile x2 y1
  , Tile x2 y2 ]

testInput :: String
testInput = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3\n"
