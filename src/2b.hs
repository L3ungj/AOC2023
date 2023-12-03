import Data.Function ((&))
import Debug.Trace (trace)
import Data.Char (isAlpha)

solve2 :: [String] -> Int -> Int -> Int -> Int
solve2 [] r g b = r*g*b
solve2 lst r g b =
    let num = read (head lst) :: Int in
    let raw_clr = head (tail lst) in
    let clr = if not (isAlpha (last raw_clr)) then
            take (length raw_clr - 1) raw_clr 
        else
            raw_clr in
    if clr == "blue" then
        solve2 (tail (tail lst)) r g (max num b)
    else if clr == "green" then
        solve2 (tail (tail lst)) r (max num g) b
    else if clr == "red" then
        solve2 (tail (tail lst)) (max num r) g b
    else
        0 -- unreachable

solve :: String -> Int
solve ln =
    let raw_mwords = words ln in
    let mwords = drop 2 raw_mwords in
    solve2 mwords 0 0 0

main = do
    contents <- readFile "fi.txt"
    map solve (lines contents) & sum & print
