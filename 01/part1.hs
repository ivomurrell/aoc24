import Data.List

main = do
  input <- readFile "input.txt"
  let lists = lines input
      [left, right] = transpose $ map (map read . words) lists
      result = sum $ zipWith (\l r -> abs $ l - r) (sort left) (sort right) :: Int
  putStrLn $ "the distance between the lists was " ++ show result ++ "!"
