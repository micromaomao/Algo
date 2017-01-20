gcdNaive :: (Integral n) => n -> n -> n
gcdNaive a b
  | a == 0 = b
  | b == 0 = a
  | a < 0 || b < 0 = error "a < 0 || b < 0"
  | otherwise = last (filter (\d -> d /= 0) [if snd (divMod a d) == 0 && snd (divMod b d) == 0 then d else 0 | d <- [1..(a+b)]])

gcdRem :: (Integral n) => n -> n -> n
gcdRem a b
  | a == 0 = b
  | b == 0 = a
  | a < 0 || b < 0 = error "a < 0 || b < 0"
  | otherwise = gcdRem b aMod
    where aMod = snd (divMod a b)
