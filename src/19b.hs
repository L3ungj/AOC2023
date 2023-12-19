import Debug.Trace (traceShowId, traceShow)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Array (Array, listArray, (!), (//), elems)

split lst spt c_lst lstlst
    | null lst = lstlst ++ [c_lst]
    | head lst == spt = split (tail lst) spt [] (lstlst ++ [c_lst])
    | otherwise = split (tail lst) spt (c_lst ++ [head lst]) lstlst

maxn = 4000

solve contents = 
    trav "in" initItems
    where
        lns = lines contents
        parseRule s = ((a, cond !! 1, n), target)
            where
                cond = takeWhile (/=':') s
                target = dropWhile (/=':') s & tail
                n = read (drop 2 cond) :: Int
                a = case head cond of 
                    'x' -> 0
                    'm' -> 1
                    'a' -> 2
                    's' -> 3

        parseRules s = (map parseRule $ s' & init, last s')
            where
                s' = split s ',' [] []
                
        parseWorkflow s = (name, rules)
            where
                name = takeWhile (/='{') s
                ruleStr = dropWhile (/='{') s & tail & init
                rules = parseRules ruleStr

        workflows = takeWhile (/="") lns & map parseWorkflow & 
            Map.fromList
        
        initItems = listArray ((0, 0), (3, maxn-1)) (replicate (4*maxn) True)

        travRules (rules, otw) items
            | null rules = trav otw items
            | otherwise = 
                travRules (tail rules, otw) disItems + 
                trav target satItems
            where
                otwRules = fromJust $ Map.lookup otw workflows
                ((a, c, n), target) = head rules
                sat i = case c of
                        '<' -> i + 1 < n
                        '>' -> i + 1 > n
                satItems = items //
                    [((a, i), False) | i <- [0..maxn-1], not $ sat i] 
                disItems = items //
                    [((a, i), False) | i <- [0..maxn-1], sat i]

        trav workflow items = 
            case workflow of 
                "R" -> 0
                "A" -> cnt items
                t -> travRules rules items
            where
                rules = fromJust $ Map.lookup workflow workflows

        cnt items = product [length $ filter id
            [items ! (a, i) | i <- [0..maxn-1]] | 
            a <- [0..3]]

main = do
    contents <- readFile "fi.txt"
    print $ solve contents