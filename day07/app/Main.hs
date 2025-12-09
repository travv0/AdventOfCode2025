{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Set (Set)
import qualified Data.Set as S

type Coords = (Int, Int)

data BeamState = BeamState
  { beams :: Set Coords
  , splitters :: Set Coords
  , splitCount :: Int }
  deriving Show

main :: IO ()
main = do
  beamState <- parseInput <$> readFile "input.txt"
  let height = maximum $ S.map snd $ splitters beamState
  let finalState = run height beamState
  putStrLn $ "Part 1: " <> show (splitCount finalState)

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

incrementBeams :: Set Coords -> Set Coords
incrementBeams beams = S.map (\(x, y) -> (x, y + 1)) beams

countCollisions :: Set Coords -> Set Coords -> Int
countCollisions splitters beams = S.size $ S.intersection beams splitters

splitCollisions :: Set Coords -> Set Coords -> Set Coords
splitCollisions splitters =
  S.fromList . concat . fmap (splitBeam splitters) . S.toList

splitBeam :: Set Coords -> Coords -> [Coords]
splitBeam splitters beam
  | S.member beam splitters = [(fst beam - 1, snd beam),
                               (fst beam + 1, snd beam)]
  | otherwise = [beam]

parseInput :: String -> BeamState
parseInput s = BeamState { beams = getCoordSet 'S' xycs
                         , splitters = getCoordSet '^' xycs
                         , splitCount = 0 }
  where xycs = [ ((x, y), c) | (y, row) <- zip [0 ..] (lines s)
                             , (x, c) <- zip [0 ..] row ]

getCoordSet :: Char -> [(Coords, Char)] -> Set Coords
getCoordSet c = S.fromList . map fst . filter ((== c) . snd)
