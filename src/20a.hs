import Debug.Trace (traceShowId, traceShow)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Array (Array, listArray, (!), range, (//), accum, assocs)
import Data.Sequence (Seq, viewl, ViewL((:<)), (<|), (|>), (><), fromList)

type Sig = (Int, Int, Bool)

split spt lst = split' lst [] []
    where
        n = length spt
        split' lst c_lst lstlst
            | null lst = lstlst ++ [c_lst]
            | take n lst == spt =
                split' (drop n lst) [] (lstlst ++ [c_lst])
            | otherwise = split' (tail lst) (c_lst ++ [head lst]) lstlst

xor a b = (a || b) && not (a && b)

solve lns = 
    let (_, _, h, l) = procN 1000 init_state init_signals 0 0
    in h*l
    where
        lns1 = map (split " -> ") lns
        nodes0 = map head lns1
        nodes = map (\nd -> (if nd == "broadcaster" then id else tail) nd) nodes0
            -- & traceShowId
        nodesN = zip nodes [0..]
        convMap = Map.fromList nodesN & traceShowId
        conv s = case Map.lookup s convMap of
            Just n -> n
            Nothing -> -1 -- the output (rx)
        tars = map (\ln -> split ", " (ln !! 1) & map conv) lns1
        bounds = (0, length nodes - 1)
        graph = listArray bounds tars & traceShowId
        amps = listArray bounds
            [head (nodes0 !! i) == '&' | i <- range bounds] & traceShowId
        bounds2 = ((0, 0), (length nodes - 1, length nodes - 1))
        adjMat = listArray bounds2
            [j `elem` graph ! i | (i, j) <- range bounds2]
        init_state = listArray bounds2
            [(not (amps ! i) && j /= 0) ||
            (amps ! i && not (adjMat ! (j, i))) |
            (i, j) <- range bounds2]
        bcn = conv "broadcaster"
        init_signals = fromList [(-1, bcn, False)]

        -- process :: Array (Int, Int) Bool -> Seq Sig -> Int -> Int -> Int
        process st q h l
            | null q = (st, h, l)
            | v == bcn = process st qbc h lbc
            | otherwise = process st' q'' h' l'
            where
                (u, v, hl) :< q' = viewl q 
                    -- & traceShowId & traceShow (h, l)
                -- bc
                qbc = q' >< fromList [(v, v', False) | 
                    v' <- graph ! v]
                lbc = l + length (graph ! v)
                -- otw
                st' = if amps ! v
                    then st // [((v, u), hl)]
                    else accum xor st [((v, 0), not hl)]
                    -- & traceShowId
                hl' = if amps ! v
                    then any not [st' ! (v, t) | t <- range bounds]
                    else st' ! (v, 0)
                send = amps ! v || not hl
                new_sigs = [(v, v', hl') | v' <- graph ! v,
                    v' >= 0, send]
                    -- & traceShowId
                h' = h + (if send && hl' then graph ! v & length else 0)
                l' = l + (if not send || hl' then 0 else graph ! v & length)
                q'' = q' >< fromList new_sigs
        
        procN n st q h l 
            | n == 0 = (st, q, h, l)
            | otherwise = procN (n-1) st' init_signals h' l'
            where
                (st', h', l') = process st q h (l+1)
main = do
    contents <- readFile "fi.txt"
    let lns = lines contents
    solve lns & print