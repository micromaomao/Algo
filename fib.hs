-- Naive algorithm, O(2^(n/2))
fibNaive :: Integral i => Int -> i
fibNaive n
  | n < 0 = error "n < 0"
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (fibNaive (n-1)) + (fibNaive (n-2))

fibListConstruct :: Integral i => [i] -> [i]
fibListConstruct [] = [0]
fibListConstruct [0] = [1, 0]
fibListConstruct (a:b:lst) = a+b:a:b:lst

fibListContinue :: Integral i => Int -> [i] -> [i]
fibListContinue l lst
  | l <= 0 = []
  | length lst >= l = lst
  | otherwise = fibListContinue l (fibListConstruct lst) 

fibList :: Integral i => Int -> i
fibList n = head (fibListContinue n [])
