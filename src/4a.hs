import Data.Function ((&))
import Debug.Trace (trace)
import Data.Bits (Bits(shift))

takeUntil :: (a -> Bool) -> [a] -> [a] -> [a]
takeUntil pred lst ret
    | pred (head lst) = reverse ret
    | otherwise = takeUntil pred (tail lst) (head lst : ret)

isElemOf :: (Eq a) => [a] -> a -> Bool
isElemOf lst a = elem a lst

solve :: (Num a) => [Char] -> a
solve ln = 
    let rwords = words ln in
    let num_str = head (tail rwords) in
    let idx = read (take (length num_str - 1) num_str) :: Int in
    let rwords1 = drop 2 rwords in
    let winNums = takeUntil (=="|") rwords1 [] & trace (show rwords1) in
    let myNums = drop (length winNums + 1) rwords1 in
    let winlen = length (filter (isElemOf winNums) myNums) in
    if winlen == 0 then 0 
    else 2 ^ (winlen - 1) 
    
main = do
    contents <- readFile "fi.txt"
    map solve (lines contents) & sum & print