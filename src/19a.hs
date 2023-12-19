import Debug.Trace (traceShowId)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

split lst spt c_lst lstlst
    | null lst = lstlst ++ [c_lst]
    | head lst == spt = split (tail lst) spt [] (lstlst ++ [c_lst])
    | otherwise = split (tail lst) spt (c_lst ++ [head lst]) lstlst

solve contents = 
    items & map (\item -> (trav "in" item, sum item)) &
        filter fst & map snd & sum
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

        parseItem s = map ((read::String->Int) . tail . dropWhile (/='=')) s'
            where 
                s' = split s ',' [] []

        workflows = takeWhile (/="") lns & map parseWorkflow & 
            Map.fromList
        items = dropWhile (/="") lns & tail & 
            map (parseItem . init)

        travRules (rules, otw) item
            | null rules = otw
            | satisfied = target
            | otherwise = travRules (tail rules, otw) item
            where
                ((a, c, n), target) = head rules
                satisfied = case c of
                    '<' -> item !! a < n
                    '>' -> item !! a > n

        trav workflow item = 
            case target of 
                "R" -> False
                "A" -> True
                t -> trav t item
            where
                rules = fromJust $ Map.lookup workflow workflows
                target = travRules rules item



main = do
    contents <- readFile "fi.txt"
    print $ solve contents