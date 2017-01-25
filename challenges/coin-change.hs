import Debug.Trace
import Data.Array

waysToChange :: Int -> [Int] -> Int
waysToChange toChange coins
  | toChange == 0 = 0
  | length coins == 0 = 0
  | minCoin > toChange = 0
  | minCoin == toChange = 1
  | otherwise =
    let dpSolutions = array (0, toChange) (oneWayForZeroAmount:zeroWaysForAllAmountTooSmall ++ oneWayForMinCoin ++ dpContinue)
        oneWayForZeroAmount = (0, 1)
        zeroWaysForAllAmountTooSmall = map (\am -> (am, 0)) [1..(minCoin - 1)]
        oneWayForMinCoin = [(minCoin, 1)]
        dpContinue = [ (i, sum $ (let a = map (\c -> dpSolutions!(i - c)) (filter (<=i) coins) in trace (show i ++ ": " ++ show a) a)) | i <- [(minCoin + 1)..toChange]]
        -- waysToCharge amount = number of choice you have at this stage + foreach. choice ...
        in trace (show (elems dpSolutions)) dpSolutions!toChange
    where minCoin = minimum coins

main = do
  l1 <- getLine
  l2 <- getLine
  let [toChange, _] = map read $ words l1
  let coins = map read $ words l2
  putStrLn $ show $ waysToChange toChange coins
