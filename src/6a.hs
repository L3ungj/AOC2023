import Data.Function ((&))
import Debug.Trace (trace)

f t x = x * (t - x)

solve t d = 
    let res = [0..t] & map (f t) & filter (> d) & length in
    res & trace (show res)

main = do
    contents <- readFile "fi.txt"
    let lns = lines contents
    let times = lns & head & words & drop 1 & map (read :: String -> Int)
    let dists = lns & last & words & drop 1 & map (read :: String -> Int)
    zipWith solve times dists & product & print