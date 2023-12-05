import Data.Function ((&))
import Debug.Trace (trace)

split [] _ c_lst lstlst = 
    lstlst ++ [c_lst]
split lst spt c_lst lstlst =
    if head lst == spt then
        split (tail lst) spt [] (lstlst ++ [c_lst])
    else
        split (tail lst) spt (c_lst ++ [head lst]) lstlst

merge1 num rule = 
    let rule0 = words rule in
    let src = read (head (tail rule0)) :: Int in
    let dest = read (head rule0) :: Int in
    let rge = read (head (tail (tail rule0))) :: Int in
    if num >= src && num < src + rge then
        -(dest + num - src) & trace (show (dest + num - src))
    else
        num & trace (show num)

merge0 num rules =
    let rules1 = drop 1 rules in 
    abs (foldl merge1 num rules1)

solve1 rules seed = 
    foldl merge0 seed rules

solve blocks = 
    let seeds = drop 1 (words (head (head blocks))) in
    let seeds1 = map (read :: String -> Int) seeds in
    map (solve1 (tail blocks)) seeds1 & minimum

main = do
    contents <- readFile "fi.txt"
    -- print (split (lines contents) "" [] [])
    solve (split (lines contents) "" [] []) & print