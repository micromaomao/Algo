import Data.Array
import Debug.Trace

editDistance :: (Eq e, Show e) => Array Int e -> Array Int e -> Int
editDistance a b =
  let lenArr arr = let b = bounds arr in snd b - fst b + 1
      getArr arr idx = let (bl, _) = bounds arr in arr!(bl + idx)
      lenA = lenArr a
      lenB = lenArr b
      matIndex (iA, iB) = (iA + 1) * (lenB + 1) + (iB + 1)
      dMatrix = array (0, matIndex (lenA - 1, lenB - 1)) (rest ++ initEdge ++ (foldl1 (++) $ map dp [0..(lenA - 1)]))
      rest = [(i, 0) | i <- [0..(matIndex (lenA - 1, lenB - 1))]]
      initEdge = [ (matIndex (iA, -1), iA + 1) | iA <- [(-1)..(lenA - 1)] ]
                  ++ [ (matIndex (-1, iB), iB + 1) | iB <- [(-1)..(lenB - 1)] ]
      dp indA = let
        currentMin indB = minimum [
            let left = dMatrix!(matIndex (indA, indB - 1)) in (left + 1), -- Deletion
            let above = dMatrix!(matIndex (indA - 1, indB)) in (above + 1), -- Inseration
            let topLeft = dMatrix!(matIndex (indA - 1, indB - 1)) in (
              if getArr a indA == getArr b indB then topLeft else (topLeft + 1) -- Replacement / Match
            )
          ]
                      in [ (matIndex (indA, indB), currentMin indB) | indB <- [0..(lenB - 1)] ]
      showMatrix ele
        | null ele = "   " ++ (unwords $ map show $ elems b)
        | otherwise =
          let (cu, rem) = splitAt (matIndex (0, -1)) ele
              in (let idx = head cu - 1 in if idx >= 0 then show $ getArr a idx else "   ") ++ " " ++ (unwords $ map show cu) ++ "\n" ++ showMatrix rem
      in trace (showMatrix $ elems dMatrix) (dMatrix!(matIndex (lenA - 1, lenB - 1)))
