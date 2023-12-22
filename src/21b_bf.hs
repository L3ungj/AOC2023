import Debug.Trace (traceShowId, traceShow)
import Data.Function ((&))
import Data.Array (elems, range, listArray, (//), (!))
import qualified Data.Set as Set

-- brute force version

-- 65: 3832
-- 196: 33967

maxMove = 196

solve grid = movest maxMove s_pos
    where
        n = length grid 
        m = length $ head grid
        h = n `div` 2
        q = maxMove `div` n

        -- inBounds x y =
        --     0 <= x && x < n && 0 <= y && y < m
        getMoves x y =
            [(x', y') |
                (dx, dy) <- [(0, 1), (0, -1), (-1, 0), (1, 0)],
                let x' = x + dx,
                let y' = y + dy,
                let (x'', y'') = (md x', md y'),
                grid !! x'' !! y'' /= '#']
        md x = (x `mod` n + n) `mod` n
        s_pos = (h, h)

        movest n st = 
            move n (Set.singleton st)

        move !n lastVis
            | n == 0 = Set.size lastVis
            | otherwise = move (n-1) vis
                where
                    upds = [getMoves i j |
                        (i, j) <- Set.elems lastVis] &
                        concat
                    vis = Set.fromList upds


main = do
    contents <- readFile "fi.txt"
    solve (lines contents) & print