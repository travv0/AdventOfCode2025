{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as S

type Coords = (Int, Int)

data Beam = Beam
  { beamCoords :: Coords
  , beamCount :: Int }
  deriving (Show, Eq, Ord)

type Beams = [Beam]
type Splitters = Set Coords

data BeamState = BeamState
  { beams :: Beams
  , splitters :: Splitters
  , splitCount :: Int }
  deriving Show

main :: IO ()
main = do
  beamState <- parseInput <$> readFile "input.txt"
  let height = maximum $ S.map snd $ splitters beamState
  let finalState = run height beamState
  putStrLn $ "Part 1: " <> show (splitCount finalState)
  putStrLn $ "Part 2: " <> show (sum $ fmap beamCount $ beams finalState)

step :: BeamState -> BeamState
step BeamState { beams, splitters, splitCount } =
  BeamState
    { beams = splitBeams
    , splitters = splitters
    , splitCount = splitCount + newSplits }
    where incrementedBeams = incrementBeams beams
          newSplits = countCollisions splitters incrementedBeams
          splitBeams = splitCollisions splitters incrementedBeams

run :: Int -> BeamState -> BeamState
run 0 bs = bs
run n bs = run (n - 1) $ step bs

incrementBeams :: Beams -> Beams
incrementBeams beams = fmap (\(Beam (x, y) n) -> Beam (x, y + 1) n) beams

countCollisions :: Splitters -> Beams -> Int
countCollisions splitters =
  S.size . S.intersection splitters . S.fromList . fmap beamCoords

splitCollisions :: Splitters -> Beams -> Beams
splitCollisions splitters = mergeBeams . concat . fmap (splitBeam splitters)

mergeBeams :: Beams -> Beams
mergeBeams = go . sort
  where go (b1@(Beam coords1 n1) : b2@(Beam coords2 n2) : bs) =
          if coords1 == coords2
             then Beam coords1 (n1 + n2) : go bs
             else b1 : go (b2 : bs)
        go b = b

splitBeam :: Splitters -> Beam -> Beams
splitBeam splitters beam@(Beam coords@(x, y) n)
  | S.member coords splitters = [Beam (x - 1, y) n, Beam (x + 1, y) n]
  | otherwise = [beam]

parseInput :: String -> BeamState
parseInput s = BeamState { beams = fmap initBeam $ getCoords 'S' xycs
                         , splitters = S.fromList $ getCoords '^' xycs
                         , splitCount = 0 }
  where xycs = [ ((x, y), c) | (y, row) <- zip [0 ..] (lines s)
                             , (x, c) <- zip [0 ..] row ]

initBeam :: Coords -> Beam
initBeam coords = Beam coords 1

getCoords :: Char -> [(Coords, Char)] -> [Coords]
getCoords c = fmap fst . filter ((== c) . snd)
