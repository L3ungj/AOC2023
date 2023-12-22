import Debug.Trace (traceShowId, traceShow)
import Data.Function ((&))
import Data.Array (elems, range, listArray, (//), (!))

-- maxMove = 26501365
maxMove = 196 -- 33417
-- expect 33967

solve grid =
    ([i*4*isq | i <- [1..q],
    let isq = if even i then isq_even else isq_odd] & sum) + 
    (q*q + q) * (osq_even + osq_odd) +
    isq_even - 
    q * (if odd q then 1 else 6)
        & traceShow (osq_even, osq_odd, isq_even, isq_odd)
        -- (3682,3806,3832,3649)
    where
        n = length grid 
        m = length $ head grid
        h = n `div` 2
        q = maxMove `div` n

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
        s_pos = (h, h)
        init_vis = listArray bounds (repeat False)
        dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
        inSq (x, y) = dist (x, y) (h, h) <= h
        corners = [(0, 0), (0, n-1), (n-1, 0), (n-1, n-1)]

        osq_odd = map (movest (h-1)) corners & traceShowId & sum
            & (+) 1 -- disgusting bottom right corner
        osq_even = map (movest (h-2)) corners & traceShowId & sum
            & (+) 6
        isq_odd = movest (h-1) s_pos
        isq_even = movest h s_pos

        movest n st = 
            move n (init_vis // [(st, True)])

        move !mv lastVis
            | mv == 0 = [1 |
                 (i, j) <- range bounds,
                 lastVis ! (i, j)] & length
            | otherwise = move (mv-1) vis
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