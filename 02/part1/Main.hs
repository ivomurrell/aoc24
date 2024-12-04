import Data.Bool (bool)
import Data.Ix (inRange)

main :: IO ()
main = do
  input <- readFile "../input.txt"
  let reports = map (map read . words) $ lines input
      result = length $ filter isSafe reports
  putStrLn $ "the number of safe reports was " ++ show result ++ "!"
  where
    isSafe =
      getSafe
        . foldr
          ( \level (wasSafe, delt, prev, first) -> do
              let newDelt = bool (level - prev) 0 first
                  gradual = first || inRange (1, 3) (abs newDelt)
                  safe = gradual && (delt == 0 || ((delt > 0) == (newDelt > 0)))
              (wasSafe && safe, newDelt, level, False)
          )
          (True, 0 :: Int, 0, True)
    getSafe (safe, _, _, _) = safe
