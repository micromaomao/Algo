maxForwardDistance :: [Float] -> Float
maxForwardDistance nums =
  getMaxProfitFrom nums (1/0) (-1/0)
  where getMaxProfitFrom left lp hprof
          | length left == 0 = hprof
          | otherwise =  getMaxProfitFrom (tail left) cul (max hprof (c - lp))
              where cul = min lp c
                    c = head left
