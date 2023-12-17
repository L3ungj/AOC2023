import Data.Function ((&))
import Debug.Trace (trace)
import Data.Char (ord)

split [] _ c_lst lstlst =
    lstlst ++ [c_lst]
split lst spt c_lst lstlst =
    if head lst == spt then
        split (tail lst) spt [] (lstlst ++ [c_lst])
    else
        split (tail lst) spt (c_lst ++ [head lst]) lstlst

solve = foldl (\ cur c -> (cur + ord c) * 17 `mod` 256) 0
    
main = do
    contents <- readFile "fi.txt"
    map solve (split contents ',' [] []) & sum & print