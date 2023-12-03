import Data.Function ((&))
import Debug.Trace (trace)
import Data.Char (isDigit)

getnum :: String -> Int -> Int
getnum [] num = num
getnum s num
    | isDigit (head s) =
        getnum (tail s) (num * 10 + (read [head s] :: Int))
    | otherwise = num

precmp :: [String] -> String -> Int -> [Int] -> [[Int]] -> [[Int]]
precmp rem_lines [] num ret_line ret_lines =
    let ret_lines1 = ret_lines ++ [ret_line] in
    if null rem_lines then
        ret_lines1
    else
        precmp (tail rem_lines) (head rem_lines) (-1) [] ret_lines1
precmp rem_lines cur_line num ret_line ret_lines
    | isDigit (head cur_line) && num >= 0 =
        precmp rem_lines (tail cur_line) num (ret_line ++ [num]) ret_lines
    | isDigit (head cur_line) && num < 0 =
        let num1 = getnum cur_line 0 in
        precmp rem_lines (tail cur_line) num1 (ret_line ++ [num1]) ret_lines
    | head cur_line == '*' =
        precmp rem_lines (tail cur_line) (-1) (ret_line ++ [-2]) ret_lines
    | otherwise =
        precmp rem_lines (tail cur_line) (-1) (ret_line ++ [-1]) ret_lines


-- solve rem_lines pline cline nline num acc
solve :: [[Int]] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int
solve rem_lines _ [] _ num acc =
    if length rem_lines == 2 then
        acc
    else
        solve (tail rem_lines) ((-1) : head rem_lines ++ [-1]) (head (tail rem_lines)) ((-1) : head (tail (tail rem_lines)) ++ [-1]) 0 acc
solve rem_lines pline cline nline num acc
    | head cline == -2 =
        let nums = [num] in
        let nums1 = nums ++ if head (tail pline) <= 0 then
                [head pline, head (tail (tail pline))]
            else
                [head (tail pline)] in
        let nums2 = nums1 ++ if head (tail nline) <= 0 then
                [head nline, head (tail (tail nline))]
            else
                [head (tail nline)] in
        let nums3 = nums2 ++ [head (tail cline)] in
        let nums4 = filter (>= 0) nums3 in
        let res = if length nums4 == 2 then
                product nums4
            else
                0 in
        let acc1 = acc + res in
        solve rem_lines (tail pline) (tail cline) (tail nline) (-1) acc1 & trace (show nums4 ++ " " ++ show res ++ " " ++ show acc1)
    | head cline >= 0 = 
        solve rem_lines (tail pline) (tail cline) (tail nline) (head cline) acc
    | otherwise =
        solve rem_lines (tail pline) (tail cline) (tail nline) (-1) acc

solve0 :: [String] -> Int
solve0 rem_lines =
    let conv = precmp rem_lines [] (-1) [] [] & drop 1 in
    let n = length (head rem_lines) in
    let dummy_line = replicate n (-1) in
    solve (dummy_line : conv ++ [dummy_line]) [] [] [] 0 0 & trace (show conv)

main = do
    contents <- readFile "fi.txt"
    solve0 (lines contents) & print