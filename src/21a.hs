import Debug.Trace (traceShowId)
import Data.Function ((&))
import Data.Array (elems, range, listArray, (//), (!))

solve grid = move 0 init_vis
    where
        n = length grid 
        m = length $ head grid
        inBounds x y =
            0 <= x && x < n && 0 <= y && y < m
        getMoves x y =
            [(x', y') |
                (dx, dy) <- [(0, 1), (0, -1), (-1, 0), (1, 0)],
                let x' = x + dx,
                let y' = y + dy,
                inBounds x' y',
                grid !! x' !! y' /= '#']
        bounds = ((0, 0), (n-1, m-1))
        s_pos = [(i, j) | (i, j) <- range bounds,
            grid !! i !! j == 'S'] & head
        init_vis = listArray bounds (repeat False) //
            [(s_pos, True)]

        move !n lastVis
            | n == 645 = elems lastVis & filter id & length
            | otherwise = move (n+1) vis
                where
                    upds = [getMoves i j |
                        (i, j) <- range bounds,
                        lastVis ! (i, j)] &
                        concat &
                        map (, True)
                    vis = listArray bounds (repeat False) //
                        upds


main = do
    contents <- readFile "fi.txt"
    solve (lines contents) & print