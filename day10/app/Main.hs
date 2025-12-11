{-# LANGUAGE TupleSections #-}

module Main where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Algorithm.Search (dijkstraAssoc)
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)

type Parser = Parsec Void String

type Diagram = Vector Bool

type Button = [Int]

type Joltage = Vector Int

main :: IO ()
main = do
  inputs <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (sum $ mapMaybe (fmap fst . (\(d, b, j) -> shortestPath d b j)) inputs)

shortestPath :: Diagram -> [Button] -> Joltage -> Maybe (Int, [Diagram])
shortestPath diagram buttons joltage = dijkstraAssoc (neighbors buttons joltage) (== diagram) $ fmap (const False) diagram

neighbors :: [Button] -> Joltage -> Diagram -> [(Diagram, Int)]
neighbors buttons _joltage diagram = fmap ((,1) . toggleLights diagram) buttons

toggleLights :: Diagram -> Button -> Diagram
toggleLights = foldl' toggleLight

toggleLight :: Diagram -> Int -> Diagram
toggleLight diagram index =
  if index >= 0 && index < V.length diagram
    then diagram V.// [(index, not (diagram V.! index))]
    else diagram

parseInput :: String -> [(Diagram, [Button], Joltage)]
parseInput = either (error . errorBundlePretty) id . parse (many parseLine) ""

parseLine :: Parser (Diagram, [Button], Joltage)
parseLine = do
  diagram <- parseDiagram <* space
  buttons <- many parseButton
  joltage <- parseJoltage <* optional eol
  return (diagram, buttons, joltage)

parseDiagram :: Parser Diagram
parseDiagram = do
  _ <- char '['
  cells <- many (char '.' <|> char '#')
  _ <- char ']'
  let diagram = V.fromList $ map (== '#') cells
  return diagram

parseButton :: Parser Button
parseButton = do
  _ <- char '('
  nums <- sepBy (read <$> some digitChar) (char ',')
  _ <- char ')' <* space
  return nums

parseJoltage :: Parser Joltage
parseJoltage = do
  _ <- char '{'
  nums <- sepBy (read <$> some digitChar) (char ',')
  _ <- char '}'
  return $ V.fromList nums
