import Debug.Trace (traceShowId)
import Data.Function ((&))

shoelace [pt] acc = abs acc `div` 2
shoelace pts acc =
    let a = head pts in
    let b = pts !! 1 in
    shoelace (tail pts) (acc + fst a * snd b - fst b * snd a)

solve lns = 
    let 
        lns' = map (\ln -> words ln & take 2
            & (\cn -> (head cn, read (cn !! 1) :: Int))) lns
        move dir n (x, y) = case dir of
            "U" -> (x, y + n)
            "D" -> (x, y - n)
            "L" -> (x - n, y)
            "R" -> (x + n, y)
        tot = sum (map snd lns') + 1
        posLst = foldl (\posLst (dir, n) -> posLst ++ [move dir n (last posLst)])
            [(0, 0)] lns'
        area = shoelace posLst 0
    in
        area + tot `div` 2 + 1
            
main = do
    contents <- readFile "fi.txt"
    solve (lines contents) & print