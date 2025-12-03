module Main where

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (sumJoltages 2 input)
  putStrLn $ "Part 2: " <> show (sumJoltages 12 input)

findLargestDigit :: String -> Maybe (Char, Int)
findLargestDigit (h:t) = Just $ findLargestDigit' 0 0 h t
findLargestDigit _ = Nothing

findLargestDigit' :: Int -> Int -> Char -> String -> (Char, Int)
findLargestDigit' i largestI largestDigit (h:t)
  | h > largestDigit = findLargestDigit' (i+1) (i+1) h t
  | otherwise = findLargestDigit' (i+1) largestI largestDigit t
findLargestDigit' _ largestI largestDigit [] = (largestDigit, largestI)

findLargestJoltage :: Int -> String -> String
findLargestJoltage 0 _ = ""
findLargestJoltage len s =
  case findLargestDigit $ take (length s - (len - 1)) s of
    Just (digit, i) -> digit : findLargestJoltage (len-1) (drop (i+1) s)
    Nothing -> ""

sumJoltages :: Int -> [String] -> Int
sumJoltages len = sum . map (read . findLargestJoltage len)
