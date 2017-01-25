data Polynomial n = Polynomial [n]

degree :: Polynomial n -> Int
degree (Polynomial p)
  | (pLen) <= 1 = 0
  | otherwise = pLen - 1
    where pLen = length p

instance (Show n, Ord n, Num n) => Show (Polynomial n) where
  show p@(Polynomial []) = ""
  show p@(Polynomial pl@(plc:rem))
    | otherwise = (if plc < 0 then "- " else "+ ") ++ (show $ abs plc) ++ (if length rem > 0 then (if length rem == 1 then "x" else "x^" ++ (show $ length rem)) ++ " " else "") ++ (show (Polynomial rem))

instance (Num n) => Num (Polynomial n) where
  a@(Polynomial al) + b@(Polynomial bl)
    | degree b < degree a = (b + a)
    | degree a == degree b = Polynomial $ map (\(an, bn) -> an + bn) $ zip al bl
    | otherwise = (padTo (degree b) a) + b
  negate (Polynomial a) = Polynomial (map negate a)
  a * b = multiplyNaive a b
  abs (Polynomial a) = Polynomial (map abs a)
  signum (Polynomial a) = Polynomial [signum $ head a]
  fromInteger i = (Polynomial [fromInteger i])

padTo :: (Num n) => Int -> (Polynomial n) -> (Polynomial n)
padTo toDeg a@(Polynomial al)
  | length al == 0 && toDeg > 0 = padTo toDeg (Polynomial (0:al))
  | cuDeg > toDeg = error $ "The Polynomial have a larger degree than " ++ (show toDeg)
  | cuDeg == toDeg = a
  | otherwise = (Polynomial $ (replicate (toDeg - cuDeg) 0) ++ (al))
    where cuDeg = degree a

-- O(n^2)

multiplyNaive :: (Num n) => Polynomial n -> Polynomial n -> Polynomial n
multiplyNaive p1@(Polynomial p1l) p2@(Polynomial p2l)
  | length p1l > length p2l = multiplyNaive p2 p1 -- Simplify stuff
  | length p1l == 0 = Polynomial [0]
  | length p1l == 1 = let c = head p1l in Polynomial ( map (*c) p2l )
  | otherwise = foldl1 (+) $ map (\(a, degRaise) -> let nP = (map (*a) p2l) ++ (replicate degRaise 0) in Polynomial nP) wIndex
                  where wIndex = zip p1l $ iterate (+(-1)) (degree p1)
