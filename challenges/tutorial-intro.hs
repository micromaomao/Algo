proc :: (Eq q) => q -> [q] -> Int
proc _ [] = 0
proc search ar
  | (head ar == search) = 0
  | otherwise = 1 + (proc search $ tail ar)

main = do
  lineA <- getLine
  let search = read lineA :: Int
  lineB <- getLine
  lineC <- getLine
  let ar = map read $ words $ lineC
  putStrLn $ show $ proc search ar
