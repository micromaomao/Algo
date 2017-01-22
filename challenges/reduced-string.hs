reduceStringIdx :: [Char] -> Int -> [Char]
reduceStringIdx str idx
  | length str <= 1 = str
  | length str <= (idx + 1) = str
  | str!!(idx) == str!!(idx + 1) = let (lf, ri) = splitAt idx str in reduceStringIdx (lf ++ (drop 2 ri)) (max 0 (idx - 1))
  | otherwise = reduceStringIdx str (idx + 1)

reducedString :: [Char] -> [Char]
reducedString str = reduceStringIdx str 0

main = do
  line <- getLine
  putStrLn $ (let rst = reducedString line in if length rst == 0 then "Empty String" else rst)
