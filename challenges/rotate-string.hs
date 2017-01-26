rotations str n
  | n >= length str = str
  | otherwise = rt ++ lf ++ " " ++ (rotations str (n + 1))
      where (lf, rt) = splitAt n str

process :: Int -> IO ()
process cn = do
  str <- getLine
  putStrLn $ rotations str 1
  if cn > 1 then process (cn - 1) else do { return () }

main = do
  l <- getLine
  let cn = read l
  process cn
