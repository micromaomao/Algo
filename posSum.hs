asSumMax :: Int -> Int -> [Int]
asSumMax startingFrom num
  | startingFrom <= 0 || num <= 0 = [0]
  | num <= startingFrom * 2 = [num]
  | otherwise = startingFrom:(asSumMax (startingFrom + 1) (num - startingFrom))
