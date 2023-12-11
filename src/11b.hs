import Data.Function ((&))
import Debug.Trace (trace)
import Data.List (transpose)

findPos graph ers ecs =
    let n = length graph in
    let m = length (head graph) in
    [(x, y) | x <- [0..n-1], y <- [0..m-1]]
        & filter (\ (x, y) -> graph !! x !! y == '#')
        & map (\ (x, y) -> (x + 999999 * length [er | er <- ers, er < x], y + 999999 * length [ec | ec <- ecs, ec < y]))

dist pos1 pos2 = abs (fst pos1 - fst pos2) + abs (snd pos1 - snd pos2)
 
solve rows = 
    let ers = filter (\ (i, row) -> all (== '.') row) (zip [0..] rows) & map fst in
    let cols = transpose rows in
    let ecs = filter (\ (i, col) -> all (== '.') col) (zip [0..] cols) & map fst in
    let stars = findPos rows ers ecs in
    let n = length stars in
    [(i, j) | i <- [0..n-1], j <- [0..n-1], j > i]
        & map (\ (i, j) -> dist (stars !! i) (stars !! j))
        & sum

main = do
    contents <- readFile "fi.txt"
    solve (lines contents) & print
