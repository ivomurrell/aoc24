import Data.List

parseLine :: String -> (Int, Int)
parseLine line =
  readLine $ words line
  where
    readLine [l, r] = (read l, read r)
    readLine _ = error $ "failed to parse line " ++ line

main :: IO ()
main = do
  input <- readFile "../input.txt"
  let (left, right) = unzip . map parseLine $ lines input
      result = sum $ zipWith (\l r -> abs $ l - r) (sort left) (sort right)
  putStrLn $ "the distance between the lists was " ++ show result ++ "!"
