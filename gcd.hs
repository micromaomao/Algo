-- Greatest common divider

-- O(a+b)
gcdNaive :: (Integral n) => (n, n) -> n
gcdNaive (a, b)
  | a == 0 = b
  | b == 0 = a
  | a < 0 || b < 0 = error "a < 0 || b < 0"
  | otherwise = last (filter (\d -> d /= 0) [if mod a d == 0 && mod b d == 0 then d else 0 | d <- [1..(a+b)]])

-- O(log(n))
gcdRem :: (Integral n) => (n, n) -> n
gcdRem (a, b)
  | a == 0 = b
  | b == 0 = a
  | a < 0 || b < 0 = error "a < 0 || b < 0"
  | otherwise = gcdRem (b, (mod a b))

lcm :: (Integral i) => (i, i) -> i
lcm (a, b)
  | a == 0 || b == 0 = 0
  | a == 1 || b == 1 = 1
  | otherwise = a * b `div` (gcd a b)
