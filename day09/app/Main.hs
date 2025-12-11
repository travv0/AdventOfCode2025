module Main where

import Data.Maybe (mapMaybe, catMaybes)
import Data.List.Split (splitOn)
import Debug.Trace

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
edgesIntersect _inner@(Edge (Tile ix1 iy1) (Tile ix2 iy2))
               _outer@(Edge (Tile ox1 oy1) (Tile ox2 oy2))
   -- both edges are vertical
  | ox1 == ox2 && ix1 == ix2 = ox1 == ix1
                               && (min oy1 oy2 < max iy1 iy2 && max oy1 oy2 > min iy1 iy2) -- overlap check
                               && (max oy1 oy2 < max iy1 iy2 || min oy1 oy2 > min iy1 iy2) -- inner extends beyond outer
  -- both edges are horizontal
  | oy1 == oy2 && iy1 == iy2 = oy1 == iy1
                               && (min ox1 ox2 < max ix1 ix2 && max ox1 ox2 > min ix1 ix2)
                               && (max ox1 ox2 < max ix1 ix2 || min ox1 ox2 > min ix1 ix2)
  -- outer is vertical, inner is horizontal
  | ox1 == ox2 = min oy1 oy2 < iy1 && iy1 < max oy1 oy2
                 && min ix1 ix2 < ox1 && ox1 < max ix1 ix2
  -- outer is horizontal, inner is vertical
  | ix1 == ix2 = min iy1 iy2 < oy1 && oy1 < max iy1 iy2
                 && min ox1 ox2 < ix1 && ix1 < max ox1 ox2
  | otherwise = error "edges must be horizontal or vertical"

anyEdgesIntersect :: Edge -> [Edge] -> Bool
anyEdgesIntersect innerEdge = any (edgesIntersect innerEdge)

rectangleInPolygon :: Tile -> Tile -> [Edge] -> Bool
rectangleInPolygon (Tile x1 y1) (Tile x2 y2) edges =
  all (`tileInPolygon` edges) sideTiles
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


calcValid :: [Tile] -> [Edge] -> [(Tile, Tile)]
calcValid tiles edges =
  [area | tile1@(Tile x1 y1) <- tiles
        , tile2@(Tile x2 y2) <- tiles
        , let area = (tile1, tile2)
        , let res = rectEdges tile1 tile2
        , tile1 < tile2
        , let intersects = any (`anyEdgesIntersect` edges) res
        -- , trace (show tile1 ++ " " ++ show tile2 ++ " intersects: " ++ show intersects) True
        , not intersects]
        -- , all (`tileInPolygon` edges) (rectCorners tile1 tile2)]
        -- , rectangleInPolygon tile1 tile2 edges]

calcValidAreas :: [Tile] -> [Edge] -> [Double]
calcValidAreas tiles edges =
  [area | tile1@(Tile x1 y1) <- tiles
        , tile2@(Tile x2 y2) <- tiles
        , let area = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
        , let res = rectEdges tile1 tile2
        , tile1 < tile2
        , let intersects = any (`anyEdgesIntersect` edges) res
        -- , trace (show tile1 ++ " " ++ show tile2 ++ " intersects: " ++ show intersects) True
        , not intersects]
        -- , all (`tileInPolygon` edges) (rectCorners tile1 tile2)]
        -- , rectangleInPolygon tile1 tile2 edges]
  -- [area | tile1@(Tile x1 y1) <- tiles
  --       , tile2@(Tile x2 y2) <- tiles
  --       , let res = rectEdges tile1 tile2
  --       , tile1 < tile2
  --       , not (any (`anyEdgesIntersect` res) edges)]
  --       -- , all (`tileInPolygon` edges) (rectCorners tile1 tile2)]
  --       -- , rectangleInPolygon tile1 tile2 edges]

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
