import Data.Function ((&))
import Debug.Trace (trace)
import Data.Char (isDigit)

isSpecial :: Char -> Bool
isSpecial c = c /= '.' && not (isDigit c)

-- solve rem_lines pline cline nline num good acc
solve :: [String] -> String -> String -> String -> Int -> Bool -> Int -> Int
solve rem_lines _ [] _ num good acc =
    let acc1 = if good then acc + num else acc in
    if length rem_lines == 2 then
        acc1
    else
        solve (tail rem_lines) ("." ++ head rem_lines ++ ".") (head (tail rem_lines)) ("." ++ head (tail (tail rem_lines)) ++ ".") 0 False acc1
solve rem_lines pline cline nline num good acc
    | head cline == '.' =
        let acc1 = if good then acc + num else acc in
        solve rem_lines (tail pline) (tail cline) (tail nline) 0 False acc1
    | not (isDigit (head cline)) =
        solve rem_lines (tail pline) (tail cline) (tail nline) 0 True (acc + num)
    | otherwise =
        let num1 = num * 10 + (read [head cline] :: Int) in
        let good1 = good ||
                any isSpecial (take 3 nline) ||
                any isSpecial (take 3 pline) in
        solve rem_lines (tail pline) (tail cline) (tail nline) num1 good1 acc

solve0 :: [String] -> Int
solve0 rem_lines = 
    let n = length (head rem_lines) in 
    let dummy_line = replicate n '.' in
    solve (dummy_line : rem_lines ++ [dummy_line]) [] [] [] 0 False 0

main = do
    contents <- readFile "fi.txt"
    solve0 (lines contents) & print