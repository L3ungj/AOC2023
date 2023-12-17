import Data.Function ((&))
import Debug.Trace (trace)
import Data.Char (ord)
import Data.List (elemIndex, delete)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

split [] _ c_lst lstlst =
    lstlst ++ [c_lst]
split lst spt c_lst lstlst =
    if head lst == spt then
        split (tail lst) spt [] (lstlst ++ [c_lst])
    else
        split (tail lst) spt (c_lst ++ [head lst]) lstlst

hash = foldl (\ cur c -> (cur + ord c) * 17 `mod` 256) 0

updBox boxes idx box =
    let n = length boxes in
    [if i == idx then box else boxes !! i | i <- [0..n-1]]

update (boxes, mp) s
    | last s == '-' =
        let s1 = init s in
        let box = boxes !! hash s1 in
        let box1 = delete s1 box in
        let mp1 = Map.delete s1 mp in
        let boxes1 = updBox boxes (hash s1) box1 in
        (boxes1, mp1)
    | otherwise =
        let s1 = takeWhile (/= '=') s in
        let val = read (drop (length s1 + 1) s) :: Int in
        let box = boxes !! hash s1 in
        let box1 = if s1 `elem` box then box else box ++ [s1] in
        let mp1 = Map.insert s1 val mp in
        let boxes1 = updBox boxes (hash s1) box1 in
        (boxes1, mp1)

main = do
    contents <- readFile "fi.txt"
    let cmds = split contents ',' [] []
    let (boxes, mp) = foldl update (replicate 256 [], Map.empty) cmds
    print boxes
    print mp
    [(i+1) * sum [(j+1) * (Map.lookup (boxes !! i !! j) mp & fromJust)
        | j <- [0..(length (boxes !! i) - 1)]]
        | i <- [0..255]] & sum & print