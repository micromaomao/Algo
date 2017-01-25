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

instance (Eq a, Num a) => Eq (Polynomial a) where
  a@(Polynomial al) == b@(Polynomial bl)
    | length bl < length al = b == a
    | length al == 0 && length bl == 0 = True
    | length al == length bl = al == bl
    | otherwise = (padTo (degree b) a) == b

padTo :: (Num n) => Int -> (Polynomial n) -> (Polynomial n)
padTo toDeg a@(Polynomial al)
  | length al == 0 && toDeg >= 0 = padTo toDeg (Polynomial (0:al))
  | cuDeg > toDeg = error $ "The Polynomial have a larger degree than " ++ (show toDeg)
  | cuDeg == toDeg = a
  | otherwise = (Polynomial $ (replicate (toDeg - cuDeg) 0) ++ (al))
    where cuDeg = degree a

polyRaise :: (Num n) => Int -> Polynomial n -> Polynomial n
polyRaise degRaise (Polynomial po) = Polynomial (po ++ (replicate degRaise 0))

-- O(n^2)

multiplyNaive :: (Num n) => Polynomial n -> Polynomial n -> Polynomial n
multiplyNaive p1@(Polynomial p1l) p2@(Polynomial p2l)
  | degree p1 > degree p2 = multiplyNaive p2 p1 -- Simplify stuff
  | length p1l == 0 = Polynomial [0]
  | length p1l == 1 = let c = head p1l in Polynomial ( map (*c) p2l )
  | otherwise = foldl1 (+) $ map (\(a, degRaise) -> let nP = (map (*a) p2l) in polyRaise degRaise $ Polynomial nP) wIndex
                  where wIndex = zip p1l $ iterate (+(-1)) (degree p1)

-- I forgot that Haskell use linked list
-- FIXME: Implementation incorrect

multiplyRecursive :: (Num n) => Polynomial n -> Polynomial n -> Polynomial n
multiplyRecursive a@(Polynomial al) b@(Polynomial bl)
  | degree a > degree b = multiplyRecursive b a -- Simplify stuff
  | length al == 0 = Polynomial [0]
  | length al == 1 = let c = head al in Polynomial ( map (*c) bl )
  | length al /= length bl = multiplyRecursive (padTo (degree b) a) b
  | otherwise =
    let (alf, art) = splitAt (div lfRaise 2) al
        (blf, brt) = splitAt (div lfRaise 2) bl
        Polynomial ablf = multiplyRecursive (Polynomial alf) (Polynomial blf)
        Polynomial abrt = multiplyRecursive (Polynomial art) (Polynomial brt)
        in (polyRaise lfRaise $ Polynomial ablf)
            + polyRaise (length alf) ((multiplyRecursive (Polynomial alf + Polynomial art) (Polynomial blf + Polynomial brt)) - Polynomial ablf - Polynomial abrt)
            + Polynomial abrt
    where lfRaise = length al
