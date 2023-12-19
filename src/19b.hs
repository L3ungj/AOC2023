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
        
        initItems = listArray (0, 4*maxn - 1) (replicate (4*maxn) True)

        travRules :: ([((Int, Char, Int), String)], String) -> Array Int Bool -> Int
        travRules (rules, otw) items
            | null rules = trav otw items
            | otherwise = 
                travRules (tail rules, otw) disItems + 
                trav target satItems
            where
                otwRules = fromJust $ Map.lookup otw workflows
                ((a, c, n), target) = head rules
                sat i = case c of
                        '<' -> i' < n
                        '>' -> i' > n
                    where i' = (i `mod` maxn) + 1
                satItems = items //
                    [(i, False) | i <- [a*maxn..(a+1)*maxn-1], not $ sat i] 
                disItems = items //
                    [(i, False) | i <- [a*maxn..(a+1)*maxn-1], sat i]

        trav :: String -> Array Int Bool -> Int
        trav workflow items = 
            case workflow of 
                "R" -> 0
                "A" -> cnt items
                t -> travRules rules items
            where
                rules = fromJust $ Map.lookup workflow workflows

        cnt items = s1 * s2 * s3 * s4
            where
                l = elems items & map (\b -> if b then 1 else 0)
                s1 = l & take maxn & sum
                s2 = l & drop maxn & take maxn & sum
                s3 = l & drop (2*maxn) & take maxn & sum
                s4 = l & drop (3*maxn) & take maxn & sum

main = do
    contents <- readFile "fi.txt"
    print $ solve contents