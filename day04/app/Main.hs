module Main where

import Data.Set (Set)
import qualified Data.Set as S

type Rolls = Set (Int, Int)

main :: IO ()
main = do
  rolls <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (countAccessibleRolls rolls)
  putStrLn $ "Part 2: " <> show (countRemovableRolls rolls)

countAccessibleRolls :: Rolls -> Int
countAccessibleRolls rolls = S.size $ S.filter (isAccessible rolls) rolls

isAccessible :: Rolls -> (Int, Int) -> Bool
isAccessible rolls (x, y) =
  adjacentRollCount < 4
    where coords = [(x', y') | x' <- [x - 1 .. x + 1],
                               y' <- [y - 1 .. y + 1],
                               x' /= x || y' /= y]
          adjacentRollCount = length $ filter (`S.member` rolls) coords

parseInput :: String -> Rolls
parseInput s = S.fromList
  [(x, y) | (y, row) <- zip [0 ..] (lines s),
            (x, c) <- zip [0 ..] row,
            c == '@']

removeRolls :: Rolls -> Rolls
removeRolls rolls = S.filter (not . isAccessible rolls) rolls

removeAllRemovableRolls :: Rolls -> Rolls
removeAllRemovableRolls rolls =
  let removedRollMap = removeRolls rolls
   in if removedRollMap == rolls
         then rolls
         else removeAllRemovableRolls removedRollMap

countRemovableRolls :: Rolls -> Int
countRemovableRolls rolls = rollsInOriginal - rollsAfterRemoval
  where rollsInOriginal = S.size rolls
        rollsAfterRemoval = S.size $ removeAllRemovableRolls rolls
