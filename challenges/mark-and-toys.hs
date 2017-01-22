import Data.List

proc :: Int -> [Int] -> Int
proc _ [] = 0
proc hasMoney prices@(mx:rm)
  | hasMoney == 0 = 0
  | mx <= hasMoney = 1 + (proc (hasMoney - mx) rm)
  | otherwise = 0

main = do
  lineA <- getLine
  let [numToys, hasMoney] = map read $ words $ lineA
  lineB <- getLine
  let prices = map read $ words $ lineB
  putStrLn $ show $ proc hasMoney (sort prices)
