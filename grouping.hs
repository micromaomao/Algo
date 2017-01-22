import Data.List

-- O(n log n) + O(n)
greedyGroup :: (Num a, Ord a) => a -> [(a, b)] -> [[(a, b)]]
greedyGroup maxDiff values = group $ sortBy fSort values
  where fSort (a1, _) (a2, _) = compare a1 a2
        group lst = firstGroup : (if length remaining /= 0 then (group remaining) else [])
          where (firstGroup, remaining) = span (\(a, _) -> a <= (fst $ head $ lst) + maxDiff) lst
