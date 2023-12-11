import Data.Function ((&))
import Debug.Trace (trace)
import Control.Exception (assert)

canGo dir st_cell ed_cell
    | dir == 0 && ( -- up
        notElem st_cell "|LJS" ||
        notElem ed_cell "|7FS") = False
    | dir == 1 && ( -- right
        notElem st_cell "-LFS" ||
        notElem ed_cell "-J7S") = False
    | dir == 2 && ( -- down
        notElem st_cell "|7FS" ||
        notElem ed_cell "|LJS") = False
    | dir == 3 && ( -- left
        notElem st_cell "-J7S" ||
        notElem ed_cell "-LFS") = False
    | otherwise = True

newPos dir (i, j)
    | dir == 0 = (i-1, j)
    | dir == 1 = (i, j+1)
    | dir == 2 = (i+1, j)
    | dir == 3 = (i, j-1)

shoelace [pt] acc = abs acc `div` 2
shoelace pts acc =
    let a = head pts in
    let b = pts !! 1 in
    shoelace (tail pts) (acc + fst a * snd b - fst b * snd a)


-- trav graph p (si, sj) acc | trace ("trav " ++ show si ++ " " ++ show sj ++ " " ++ show acc) False = undefined
trav graph (pi, pj) (si, sj) ret =
    if not (null ret) && graph !! si !! sj == 'S' then
        let area = shoelace (reverse ((si, sj) : ret)) 0 in
        area - length ret `div` 2 + 1
    else
    let inBounds (x, y) = 0 <= x && x < length graph && 0 <= y && y < length (head graph) in
    let go dir = if inBounds (ni, nj) && (ni, nj) /= (pi, pj) && canGo dir (graph !! si !! sj) (graph !! ni !! nj)
        then dir else -1 where (ni, nj) = newPos dir (si, sj) in
    let dir = [0..3] & map go & filter (/= -1) & head in
    trav graph (si, sj) (newPos dir (si, sj)) ((si, sj) : ret)

main = do
    contents <- readFile "fi.txt"
    let graph = lines contents
    let si = 92
    let sj = 43
    let _ = assert (graph !! si !! sj == 'S') 0
    trav (lines contents) (-1, -1) (si, sj) [] & print
