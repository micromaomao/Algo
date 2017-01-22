import Data.List

proc :: Int -> [Int] -> Int -> Int
proc numBuyer prices time
  | length prices <= numBuyer = sum $ map (*time) prices
  | otherwise = (sum $ map (*time) cu) + proc numBuyer rem (time + 1)
    where (cu, rem) = splitAt numBuyer prices

main = do
  lineA <- getLine
  let [n, k] = map read $ words lineA
  lineB <- getLine
  let costs = map read $ words lineB
  putStrLn $ show $ proc k (reverse $ sort costs) 1
