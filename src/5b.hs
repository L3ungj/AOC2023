import Data.Function ((&))
import Debug.Trace (trace)

split [] _ c_lst lstlst =
    lstlst ++ [c_lst]
split lst spt c_lst lstlst =
    if head lst == spt then
        split (tail lst) spt [] (lstlst ++ [c_lst])
    else
        split (tail lst) spt (c_lst ++ [head lst]) lstlst

applyRule :: ([(Int, Int)], [(Int, Int)]) -> [Char] -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
applyRule ([], p_done) rule ret_seeds ret_done = (ret_seeds, ret_done ++ p_done)
applyRule seeds rule ret_seeds ret_done =
    let seeds0 = fst seeds in
    let rule0 = words rule in
    let src = read (head (tail rule0)) :: Int in
    let dest = read (head rule0) :: Int in
    let rge = read (head (tail (tail rule0))) :: Int in
    let seed = head seeds0 in
    let seedl = fst seed in
    let seedr = seedl + snd seed in
    let segl = src in
    let segr = src + rge in
    if seedr <= segl || seedl >= segr then
        applyRule (tail seeds0, snd seeds) rule (head seeds0 : ret_seeds) ret_done
    else
        let splitl = (seedl, segl - seedl) in
        let splitr = (segr, seedr - segr) in
        let splitm = (max segl seedl, min segr seedr - max segl seedl) in
        let convm = (fst splitm - segl + dest, snd splitm) in
        let ret_done1 = ret_done ++ [convm] in
        let ret_seeds1 = ret_seeds ++ ([splitl | snd splitl > 0]) in
        let ret_seeds2 = ret_seeds1 ++ ([splitr | snd splitr > 0]) in
        applyRule (tail seeds0, snd seeds) rule ret_seeds2 ret_done1

merge0 seeds rules =
    let applyRule0 a b = applyRule a b [] [] in
    let res = foldl applyRule0 (seeds, []) rules in
    let res2 = uncurry (++) res in
    res2 & trace (show res2)

parse2 [] ret = ret
parse2 seeds ret =
    parse2 (tail (tail seeds)) ((head seeds, head (tail seeds)) : ret)

solve blocks =
    let seeds = drop 1 (words (head (head blocks))) in
    let seeds1 = map (read :: String -> Int) seeds in
    let seeds2 = parse2 seeds1 [] in
    let rules = tail blocks & trace (show seeds2) in
    let rules1 = map (drop 1) rules in
    foldl merge0 seeds2 rules1 & map fst & minimum

main = do
    contents <- readFile "fi.txt"
    -- print (split (lines contents) "" [] [])
    solve (split (lines contents) "" [] []) & print