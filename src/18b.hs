import Debug.Trace (traceShowId)
import Data.Function ((&))
import Data.Char (ord, isDigit)

shoelace [pt] acc = abs acc `div` 2
shoelace pts acc =
    let a = head pts in
    let b = pts !! 1 in
    shoelace (tail pts) (acc + fst a * snd b - fst b * snd a)

toDec s = 
    toDec' s 0
    where
        toDec' s acc
            | null s = acc
            | otherwise = 
                let acc' = acc * 16 +  
                        if isDigit (head s) then ord (head s) - ord '0'
                        else ord (head s) - ord 'a' + 10 in
                toDec' (tail s) acc'

solve lns = 
    let 
        lns' = map (\ln -> words ln & last & 
            drop 2 & init &
            (\s -> (last s, toDec (init s)))) lns & traceShowId
        move dir n (x, y) = case dir of
            '3' -> (x, y + n)
            '1' -> (x, y - n)
            '2' -> (x - n, y)
            '0' -> (x + n, y)
        tot = sum (map snd lns') + 1
        posLst = foldl (\posLst (dir, n) -> posLst ++ [move dir n (last posLst)])
            [(0, 0)] lns'
        area = shoelace posLst 0
    in
        area + tot `div` 2 + 1
            
main = do
    contents <- readFile "fi.txt"
    solve (lines contents) & print