import Data.Function ((&))
import Debug.Trace (trace)

getDiff [] ret = reverse (tail ret)
getDiff nums [] = getDiff (tail nums) [head nums]
getDiff nums ret = 
    let ret1 = head nums : (head nums - head ret) : tail ret in
    getDiff (tail nums) ret1

solve nums
    | all (==0) nums = 0
    | otherwise = head nums - solve (getDiff nums [])

main = do
    contents <- readFile "fi.txt"
    lines contents
        & map (\ln -> words ln & map (read::String -> Int) & solve)
        & sum & print
