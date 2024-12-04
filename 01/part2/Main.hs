parseLine :: String -> (Int, Int)
parseLine line =
  readLine $ words line
  where
    readLine [l, r] = (read l, read r)
    readLine _ = error $ "failed to parse line " ++ line

main :: IO ()
main = do
  input <- readFile "../input.txt"
  let lists = lines input
      (left, right) = unzip $ map parseLine lists
      result = sum $ map (\x -> x * length (filter (== x) right)) left
  putStrLn $ "the similarity score was " ++ show result ++ "!"
