import Data.Function ((&))
import Debug.Trace (trace)

getDiff [] ret = reverse (tail ret)
getDiff nums [] = getDiff (tail nums) [head nums]
getDiff nums ret = 
    let ret1 = head nums : (head nums - head ret) : tail ret in
    getDiff (tail nums) ret1

solve nums acc
    | all (==0) nums = acc & trace (show acc)
    | otherwise = solve (getDiff nums []) (acc+last nums)

main = do
    contents <- readFile "fi.txt"
    -- print (getDiff [1, 2, 3] [])
    lines contents
        & map (\ln -> words ln & map (read::String -> Int) & (`solve` 0))
        & sum & print
