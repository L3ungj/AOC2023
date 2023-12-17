import Data.Function ((&))
import Debug.Trace (trace)
import Data.List (transpose, replicate)
import qualified Data.Map as Map
import Control.Exception (assert)

split [] _ c_lst lstlst =
    lstlst ++ [c_lst]
split lst spt c_lst lstlst =
    if head lst == spt then
        split (tail lst) spt [] (lstlst ++ [c_lst])
    else
        split (tail lst) spt (c_lst ++ [head lst]) lstlst

westify = map east
    where
        east row =
            split row '#' [] []
                & map (\s -> foldr (:) "" ['O' | c <- s, c == 'O']
                    ++ foldr (:) "" ['.' | c <- s, c == '.'])
                & foldl1 (\ a b -> a ++ "#" ++ b)

eastify = map reverse . westify . map reverse

northify = transpose . westify . transpose

southify = transpose . eastify . transpose

cyclify = eastify . southify . westify . northify

findcyc grid =
    findcyc' Map.empty grid 1

findcyc' memo grid step = 
    let grid1 = cyclify grid in
    case Map.lookup grid1 memo of
        Just x -> (x, step - x)
        Nothing -> findcyc' (Map.insert grid1 step memo) grid1 (step + 1)

cycN grid n = 
    let grid1 = iterate cyclify grid !! n in
    let n = length grid in
    let m = length (head grid) in
    [n - i | i <- [0..n-1], j <- [0..m-1], grid1 !! i !! j == 'O'] & sum
    
main = do
    contents <- readFile "fi.txt"
    let grid = lines contents
    let (a, n) = findcyc grid
    -- print (a, n)
    cycN grid (a + (1000000000 - a) `mod` n) & print
