import Data.List

main = do
  input <- readFile "input.txt"
  let lists = lines input
      [left, right] = transpose $ map (map read . words) lists
      result = sum $ map (\x -> x * length (filter (== x) right)) left
  putStrLn $ "the similarity score was " ++ show result ++ "!"
