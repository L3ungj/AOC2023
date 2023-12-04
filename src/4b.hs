import Data.Function ((&))
import Debug.Trace (trace)
import Data.Bits (Bits(shift))

takeUntil :: (a -> Bool) -> [a] -> [a] -> [a]
takeUntil pred lst ret
    | pred (head lst) = reverse ret
    | otherwise = takeUntil pred (tail lst) (head lst : ret)

isElemOf :: (Eq a) => [a] -> a -> Bool
isElemOf lst a = elem a lst

solve :: [[Char]] -> [Int] -> Int -> Int
solve [] _ acc = acc
solve rem_lines lst acc = 
    let rwords = words (head rem_lines) in
    let num_str = head (tail rwords) in
    let idx = read (take (length num_str - 1) num_str) :: Int in
    let rwords1 = drop 2 rwords in
    let winNums = takeUntil (=="|") rwords1 [] & trace (show rwords1) in
    let myNums = drop (length winNums + 1) rwords1 in
    let winlen = length (filter (isElemOf winNums) myNums) in
    let lst1 = tail lst in
    let lst2 = map (+head lst) (take winlen lst1) ++ drop winlen lst1 in
    solve (tail rem_lines) lst2 (acc + head lst)

solve0 contents = 
    let n = length (lines contents) in
    solve (lines contents) (replicate n 1) 0
    
main = do
    contents <- readFile "fi.txt"
    solve0 contents & print