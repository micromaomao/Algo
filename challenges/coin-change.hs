import Debug.Trace
import Data.Array

waysToChange :: Int -> [Int] -> Int
waysToChange toChange coins
  | toChange == 0 = 0
  | length coins == 0 = 0
  | minCoin > toChange = 0
  | minCoin == toChange = 1
  | otherwise =
    let dMatrix = array (0, matIndex toChange (length coins)) (oneForZero ++ zeroForNoCoins ++ dpRest)
        matIndex amount useNCoins = useNCoins * (toChange + 1) + amount
        oneForZero = [ (matIndex 0 i, 1) | i <- [0..(length coins)] ]
        zeroForNoCoins = [ (matIndex i 0, 0) | i <- [1..toChange] ]
        dpRest = foldl1 (++) [ dpForAmount am | am <- [1..toChange] ]
        dpForAmount am = [ (matIndex am useNCoins, (
            let notUsingThisCoin = dMatrix!(matIndex am (useNCoins - 1))
                usingThisCoin = if (am - thisCoin) >= 0 then dMatrix!(matIndex (am - thisCoin) useNCoins) else 0
                thisCoin = coins!!(useNCoins - 1)
                in notUsingThisCoin + usingThisCoin
          )) | useNCoins <- [1..(length coins)] ]
        in dMatrix!(matIndex toChange (length coins))
    where minCoin = minimum coins

main = do
  l1 <- getLine
  l2 <- getLine
  let [toChange, _] = map read $ words l1
  let coins = map read $ words l2
  putStrLn $ show $ waysToChange toChange coins
