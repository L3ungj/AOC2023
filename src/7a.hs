import Data.Function ((&))
import Debug.Trace (trace)
import Data.Char (isDigit, ord)
import Data.List (sortBy)

conv c
    | isDigit c = ord c - ord '0' 
    | c == 'T' = 10
    | c == 'J' = 11
    | c == 'Q' = 12
    | c == 'K' = 13
    | c == 'A' = 14
    | otherwise = 0

conv1 ln = 
    let wds = words ln in
    let hand = head wds & map conv in
    let bid = read (last wds) :: Int in
    (hand, bid)

getType hand = 
    let count hand num = hand & filter (==num) & length in
    let cnts = [2..14] & map (count hand) in
    let max_cnt = maximum cnts in
    let cnt_2 = count cnts 2 in
    case () of
      _ | max_cnt == 5 -> 6
        | max_cnt == 4 -> 5
        | max_cnt == 3 && cnt_2 == 1 -> 4
        | max_cnt == 3 -> 3
        | cnt_2 == 2 -> 2
        | cnt_2 == 1 -> 1
        | otherwise -> 0
    
cmp (hand1, _) (hand2, _) =
    let t1 = getType hand1 in
    let t2 = getType hand2 in
    if t1 /= t2 then compare t1 t2
    else compare hand1 hand2

main = do
    contents <- readFile "fi.txt"
    lines contents & map conv1 & sortBy cmp 
        & zipWith (\ idx (_, bid) -> idx * bid) [1..]
        & sum & print
