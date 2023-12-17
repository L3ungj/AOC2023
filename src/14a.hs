import Data.Function ((&))
import Debug.Trace (trace)
import Data.List (transpose)

solve ln = 
    solve' ln (length ln) (length ln) 0

solve' [] _ _ acc = acc
solve' ln cur cur_O acc
    | head ln == '#' = solve' (tail ln) (cur - 1) (cur - 1) acc
    | head ln == 'O' = solve' (tail ln) (cur - 1) (cur_O - 1) (acc + cur_O)
    | otherwise = solve' (tail ln) (cur - 1) cur_O acc

main = do
    contents <- readFile "fi.txt"
    let grid = transpose (lines contents)
    map solve grid & sum & print