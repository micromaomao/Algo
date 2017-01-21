-- Fibonacci number
-- Input: n
-- Output: nth fibonacci number

-- Naive algorithm, O(2^(n/2))
fibNaive :: Integral i => Int -> i
fibNaive n
  | n < 0 = error "n < 0"
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (fibNaive (n-1)) + (fibNaive (n-2))

-- Fast algorithm, O(n^2) -- n^2 because adding big numbers is O(n) not O(1)
fibList :: Integral i => Int -> i
fibList 0 = 0
fibList n = fst ((iterate fibFromLastTwo (1, 0)) !! (n - 1))
  where fibFromLastTwo (b, a) = (a + b, b)

-- O(n)
fibLastDigit :: Int -> Int
fibLastDigit 0 = 0
fibLastDigit n = fst (head $ drop (n - 1) (iterate fibLastDigitFromLastTwo (1, 0)))
  where fibLastDigitFromLastTwo (b, a) = ((a + b) `mod` 10, b)

-- Mostly constant time
fibMod :: Int -> Int -> Int
fibMod mo n
  | mo <= 1 = 0
  | otherwise = period !! (n `mod` (length period))
    where period = fibModPeriod mo

fibModFromLastTwo :: Int -> (Int, Int) -> (Int, Int)
fibModFromLastTwo mo (b, a) = ((a + b) `mod` mo, b)

fibModInf :: Int -> [Int]
fibModInf mo | mo <= 1 = repeat 0
fibModInf mo = 0 : moLst
  where moLst = map fst $ iterate (fibModFromLastTwo mo) (1, 0)

fibModPeriod :: Int -> [Int]
fibModPeriod mo = takePeriod 3 infLst
  where infLst = fibModInf mo
        takePeriod index lst | take 3 lst /= [0, 1, 1] = error "Invalid list"
        takePeriod index lst = if ((take 3 (drop index lst)) == [0, 1, 1]) then (take index lst) else (takePeriod (index+1) lst)

fibSumMod :: Int -> Int -> Int
fibSumMod mo sumTo | sumTo <= 1 = 0
fibSumMod mo sumTo = theSum $ iterate fibSumIterate (1, 1, 0) !! (sumTo - 1)
  where fibSumIterate (sum, lastB, lastA) = (let nFib = (lastB + lastA) `mod` mo in ((sum + nFib) `mod` mo, nFib, lastB))
        theSum (sum,_,_) = sum
