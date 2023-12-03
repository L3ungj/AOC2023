import Data.Function ((&))
import Debug.Trace (trace)
import Data.Char (isAlpha)
check :: [String] -> Bool
check [] = True
-- check lst | trace (show lst) False = undefined
check lst =
    let num = read (head lst) :: Int in
    let raw_clr = head (tail lst) in
    let clr = if not (isAlpha (last raw_clr)) then
            take (length raw_clr - 1) raw_clr 
        else
            raw_clr in
    ((clr == "blue" && num <= 14) ||
        (clr == "green" && num <= 13) ||
        (clr == "red" && num <= 12)) &&
        check (tail (tail lst))

solve :: String -> Int
solve ln =
    let raw_mwords = words ln in
    let mwords = drop 2 raw_mwords in
    let idx = head (tail raw_mwords) in
    let idx1 = read (take (length idx - 1) idx) :: Int in
    if check mwords then
        idx1
    else
        0

main = do
    contents <- readFile "fi.txt"
    map solve (lines contents) & sum & print
