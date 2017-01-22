miniMaxSum :: [Int] -> [Int]
miniMaxSum nums = [minimum sums, maximum sums]
  where sums = map sumExpect [0..(length nums - 1)]
        sumExpect idx = (sum $ fst splitd) + (sum $ drop 1 $ snd splitd)
          where splitd = splitAt idx nums

main = do
  line <- getLine
  putStrLn $ unwords $ map show $ miniMaxSum $ map read (words line)
