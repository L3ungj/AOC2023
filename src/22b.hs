import Debug.Trace (traceShowId, traceShow)
import Data.Function ((&))
import Data.List (sortBy, nub)
import Data.Tuple (swap)
import Data.Array (listArray, range, (!), (//), elems)

split spt lst = split' lst [] []
    where
        n = length spt
        split' lst c_lst lstlst
            | null lst = lstlst ++ [c_lst]
            | take n lst == spt =
                split' (drop n lst) [] (lstlst ++ [c_lst])
            | otherwise = split' (tail lst) (c_lst ++ [head lst]) lstlst

mycmp ((_,_,z1), _) ((_,_,z2), _) = compare z1 z2

getBlocks ((x1, y1, z1), (x2, y2, z2)) = [(x, y, z) | x <- [xl..xr], y <- [yl..yr], z <- [z1..z2]]
    where
        (xl, xr) = (x1, x2) & (if x1 > x2 then swap else id)
        (yl, yr) = (y1, y2) & (if y1 > y2 then swap else id)

solve contents = process init_grid init_re init_blocks
    where
        init_blocks = lines contents &
            map (\s ->
                let
                    [c1, c2] = split "~" s
                    [x1, y1, z1] = split "," c1 & map (read :: String -> Int)
                    [x2, y2, z2] = split "," c2 & map (read :: String -> Int)
                    (z1', z2') = (z1, z2) & (if z1 > z2 then swap else id)
                in ((x1, y1, z1'), (x2, y2, z2'))) &
            sortBy mycmp
        n = length init_blocks
        bounds = ((0, 0, 0), (9, 9, 350))
        init_grid = listArray bounds [if z == 0 then -1 else 0 | (_, _, z) <- range bounds]
        init_deps = listArray (1, n) (repeat [])
        process grid deps blocks
            | null blocks = elems re & filter id & length & traceShow (reverse $ elems re)
            | otherwise = drop1 block
            where
                block = head blocks
                drop1 ((x1, y1, z1), (x2, y2, z2))
                    | null depLst =
                        drop1 ((x1, y1, z1 - 1), (x2, y2, z2 - 1))
                    | otherwise =
                        process grid' deps' (tail blocks)
                    where
                        depLst = map (grid !) (getBlocks ((x1, y1, z1 - 1), (x2, y2, z2 - 1))) &
                            filter (/=0) &
                            nub
                        grid' = grid // [(b, length blocks) | b <- getBlocks ((x1, y1, z1), (x2, y2, z2))]
                        deps' = deps // [(head depLst, False) | length depLst == 1, head depLst /= -1]

main = do
    contents <- readFile "fi.txt"
    solve contents & print