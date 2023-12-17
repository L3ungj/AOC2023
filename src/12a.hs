import Data.Function ((&))
import Debug.Trace (trace)
import Data.Array ((!), listArray, Ix (range))

split lst spt c_lst lstlst
  | null lst = lstlst ++ [c_lst]
  | head lst == spt = split (tail lst) spt [] (lstlst ++ [c_lst])
  | otherwise = split (tail lst) spt (c_lst ++ [head lst]) lstlst

solve ln =
    memo ! (0, 0) 
        -- & trace (show memo)
    where
    wds = words ln
    s = head wds
    springs = split (last wds) ',' [] [] & map (read::String->Int)
    (n, m) = (length s, length springs)
    memo = listArray bounds [f i j | (i, j) <- range bounds]
    bounds = ((0, 0), (n+1, m))
    -- f i j = number of ways cons from right at idx i at spring j
    f i j
        | i >= n && j == m = 1
        | i >= n = 0
        | j == m = if '#' `notElem` drop i s then 1 else 0
        | otherwise = 
            let ni = i + springs !! j in
            (if (ni > n) ||
                elem '.' (drop i s & take (springs !! j)) ||
                (ni < n && s !! ni == '#') then 0 -- cannot put spring
            else memo ! (i + springs !! j + 1, j + 1)) + -- put spring
            (if s !! i /= '#' then memo ! (i + 1, j) else 0) -- skip  
            
main = do
    contents <- readFile "fi.txt"
    map solve (lines contents) & sum & print