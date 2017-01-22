import Data.List

proc radioRange locations = groupNum $ sort locations
  where
    groupNum [] = 0
    groupNum ls = 1 + (groupNum remaining)
      where
        (currentGroup, remaining) = span (radioInReach radioPos) ls
          where radioInReach rPos house = abs (house - rPos) <= radioRange
                radioPos = last $ takeWhile firstHouseInReach ls
                  where firstHouseInReach radioPos = radioInReach radioPos (head ls)

main = do
  lineA <- getLine
  let [n, k] = map read $ words lineA
  lineB <- getLine
  let locations = map read $ words lineB
  putStrLn $ show $ proc k locations
