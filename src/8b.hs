import Data.Function ((&))
import Debug.Trace (trace)
import Data.Map (Map, fromList, keys, (!))

parse desc =
    let wds = words desc in
    let u = head wds in
    let v1 = (wds !! 2) & tail & init in
    let v2 = last wds & init in
    (u, (v1, v2))

trav graph moves cur acc 
    | last cur == 'Z' = acc
    | otherwise = 
        let dsts = graph ! cur in
        let dst = if head moves == 'L' then fst dsts else snd dsts in
        trav graph (tail moves) dst (acc+1)

solve contents =
    let lns = lines contents in
    let moves = head lns in
    let graph = drop 2 lns in
    let graph1 = graph & map parse & fromList in
    let sts = keys graph1 & filter (\ u -> last u == 'A') in
    sts & map (\ u -> trav graph1 (repeat moves & foldr1 (++)) u 0) 
    -- observe that the elements are multiples of the length of the moves
        & foldr1 lcm

main = do
    contents <- readFile "fi.txt"
    solve contents & print