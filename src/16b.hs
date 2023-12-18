import Debug.Trace (traceShowId)
import Data.Function ((&))
import Data.Array (listArray, (!), range, (//), elems)

solve grid =
    [process iv init_ene im |
        im <- init_moves,
        let iv = init_vis // [(head im, True)]] & maximum
    where
        n = length grid
        m = length (head grid)
        grid_bounds = ((0, 0), (n-1, m-1))
        dir_bounds = ((0, 0, 0), (n-1, m-1, 3))
        init_ene = listArray grid_bounds [False | _ <- range grid_bounds]
        -- dir: right 0, then clockwise
        go k i j = (i + di, j + dj)
            where (di, dj) = [(0, 1), (1, 0), (0, -1), (-1, 0)] !! k
        inBounds i j = i >= 0 && i < n && j >= 0 && j < m
        init_moves = [[(0, j, 1)] | j <- [0..m-1]] ++
            [[(i, m-1, 2)] | i <- [0..n-1]] ++
            [[(n-1, j, 3)] | j <- [0..m-1]] ++
            [[(i, 0, 0)] | i <- [0..n-1]]
        init_vis = listArray dir_bounds [False | _ <- range dir_bounds]
        getDirs c k = case c of
            '.' -> [k]
            '/' -> -- 0 <-> 3, 1 <-> 2
                if k `elem` [1, 3] then [(k+1) `mod` 4]
                else [(k+3) `mod` 4]
            '\\' -> -- 0 <-> 1, 2 <-> 3
                if k `elem` [0, 2] then [(k+1) `mod` 4]
                else [(k+3) `mod` 4]
            '-' -> -- split if 1 or 3
                if k `elem` [1, 3] then [(k+1) `mod` 4, (k+3) `mod` 4]
                else [k]
            '|' -> -- split if 0 or 2
                if k `elem` [0, 2] then [(k+1) `mod` 4, (k+3) `mod` 4]
                else [k]

        process vis ene moves
            | null moves = [if b then 1 else 0 | b <- elems ene] & sum & traceShowId
            | otherwise = process vis' ene' moves''
            where
                (i, j, k) = head moves
                moves' = tail moves
                ene' = ene // [((i, j), True)]
                newMoves = [(i', j', k') |
                    k' <- getDirs (grid !! i !! j) k,
                    let (i', j') = go k' i j,
                    inBounds i' j',
                    not (vis ! (i', j', k'))]
                vis' = vis // [((i', j', k'), True) |
                    (i', j', k') <- newMoves]
                moves'' = newMoves ++ moves'

main = do
    contents <- readFile "fi.txt"
    let grid = lines contents
    solve grid & print