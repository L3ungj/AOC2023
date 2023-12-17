import Data.Function ((&))
import Debug.Trace (trace)
import Data.List (transpose)

split [] _ c_lst lstlst =
    lstlst ++ [c_lst]
split lst spt c_lst lstlst =
    if head lst == spt then
        split (tail lst) spt [] (lstlst ++ [c_lst])
    else
        split (tail lst) spt (c_lst ++ [head lst]) lstlst

solve grid ban =
    let n = length grid in
    let m = length (head grid) in
    let grid_t = transpose grid in
    let inBounds_r i = i >= 0 && i < n in
    let chk_r i = all (\r -> not (inBounds_r (2*i+1-r)) || (grid !! r == grid !! (2*i+1-r))) [0..n-1] in
    let inBounds_c j = j >= 0 && j < m in
    let chk_c j = all (\c -> not (inBounds_c (2*j+1-c)) || (grid_t !! c == grid_t !! (2*j+1-c))) [0..m-1] in
    let res = ([(i+1)*100 | i <- [0..n-2], chk_r i, (i+1)*100 /= ban] & sum)
            + ([j+1 | j <- [0..m-2], chk_c j, j+1 /= ban] & sum) in
    res
            -- & trace (show res)

flp c = if c == '#' then '.' else '#'

solve0 grid = 
    let n = length grid in
    let m = length (head grid) in
    let ban = solve grid (-1) in 
    let grid_e x y = [[(if (i, j) == (x, y) then flp else id) (grid !! i !! j)
            | j <- [0..m-1]] | i <- [0..n-1]] in
    let res = [solve (grid_e i j) ban | i <- [0..n-1], j <- [0..m-1]] & sum in
    res `div` 2 & trace ("fin" ++ show res)

main = do
    contents <- readFile "fi.txt"
    let tests = split (lines contents) "" [] []
    map solve0 tests & sum & print