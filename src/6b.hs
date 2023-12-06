import Data.Function ((&))
import Debug.Trace (trace)

f t x = x * (t - x)

solve t d = 
    [0..t] & map (f t) & filter (> d) & length

main = do
    contents <- readFile "fi.txt"
    let lns = lines contents
    let t = lns & head & words & drop 1 & foldl1 (++) & (read :: String -> Int)
    let d = lns & last & words & drop 1 & foldl1 (++) & (read :: String -> Int)
    solve t d & print