import Data.Function ((&))
import Debug.Trace (trace, traceShowId)
import Data.Array ((!), listArray, Ix (range), (//), Array)
import Data.Char (digitToInt)
import qualified Data.Heap as H

type Node = (Int, Int, Int, Int)

solve :: [[Int]] -> Int
solve grid = 
    res
    where
    n = length grid
    m = length (head grid)
    bounds = ((0, 0, 0, 0), (n-1, m-1, 3, 6))
    -- dir: right 0, then clockwise
    go i j k t = (i + t*di, j + t*dj)
        where (di, dj) = [(0, 1), (1, 0), (0, -1), (-1, 0)] !! k
    inBounds i j = i >= 0 && i < n && j >= 0 && j < m
    initQueue = H.fromList [(0, (0, 0, k, 0)) | k <- [2..3]]
    initDist = listArray bounds [-1 | _ <- range bounds]
    resDist = dijkstra initDist initQueue
    res = minimum [d | k <- [0..3], l <- [0..6],
        let d = resDist ! (n-1, m-1, k, l), d /= -1]
    
    dijkstra :: Array Node Int -> H.MinPrioHeap Int Node -> Array Node Int
    dijkstra dist pq = 
        case H.view pq of
            Nothing -> dist
            Just ((d, (_i, j, k, l)), pq') ->
                if (i, j) == (n-1, m-1) then dist
                else if dist ! (i, j, k, l) /= -1 
                    && dist ! (i, j, k, l) < d
                        then dijkstra dist pq'
                else dijkstra dist' pq''
                where
                    i = _i
                        & trace (show (d, (_i, j, k, l)))
                    neighbours = 
                        [(i', j', k, l+1) |
                            let (i', j') = go i j k 1,
                            l < 6,
                            inBounds i' j'] ++
                        [(i', j', k', 0) |
                            k' <- [0..3],
                            k' /= k, k' /= (k+2) `mod` 4,
                            let (i', j') = go i j k' 4,
                            inBounds i' j']
                                -- & traceShowId
                    (pq'', dist') = foldr (\(i', j', k', l') (pq, dist) ->
                        let d' = 
                                if l' > 0 then d + grid !! i' !! j'
                                else d + sum [grid !! i'' !! j'' | 
                                    t <- [1..4], 
                                    let (i'', j'') = go i j k' t]
                        in
                        if dist ! (i', j', k', l') == -1 
                            || dist ! (i', j', k', l') > d' then
                                (H.insert (d', (i', j', k', l')) pq, 
                                    dist // [((i', j', k', l'), d')])
                        else (pq, dist))
                        (pq', dist) neighbours              

main = do
    contents <- readFile "fi.txt"
    let grid = lines contents & map (map digitToInt)
    solve grid & print