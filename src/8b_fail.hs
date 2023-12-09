import Data.Function ((&))
import Debug.Trace (trace)
import Data.Map (Map, fromList, keys, (!))
import Data.List (intersect)

parse desc =
    let wds = words desc in
    let u = head wds in
    let v1 = (wds !! 2) & tail & init in
    let v2 = last wds & init in
    (u, (v1, v2))

trav travMoveDict move_length curs acc = 
    let zdst = curs & map (travMoveDict !) in
    let zs = map fst zdst & foldr1 intersect in
    let dsts = map snd zdst in
    if null zs then trav travMoveDict move_length dsts (acc+move_length)
    else acc + minimum zs

travMove graph [] idx ret cur = (reverse ret, cur)
travMove graph moves idx ret cur = 
    let dst = graph ! cur & (if head moves == 'L' then fst else snd) in
    let ret1 = if last cur == 'Z' then idx:ret else ret in
    travMove graph (tail moves) (idx+1) ret1 dst

solve contents =
    let lns = lines contents in
    let moves = head lns in
    let graph = drop 2 lns in
    let graph1 = graph & map parse & fromList in
    let sts = graph1 & keys & filter (\ s -> last s == 'A') in
    let travMoveDict = keys graph1 & map (\ u -> (u, travMove graph1 moves 0 [] u)) & fromList in
    trav travMoveDict (length moves) sts 0

main = do
    contents <- readFile "fi.txt"
    solve contents & print