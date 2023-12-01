import Data.Char (isDigit)
import Data.Function ( (&) )

convFirst :: String -> [(String, Int)] -> Bool -> Int
convFirst s [] b = 0
convFirst s conv b =
  let (preword, num) = head conv in
  let word = if b then reverse preword else preword in
  if take (length word) s == word then
    num
  else
    convFirst s (tail conv) b

getFirst :: String -> Bool -> Int
getFirst [] b = 0
getFirst s b =
  if isDigit (head s) then
    read [head s] :: Int
  else
    let conv = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)] in
    let val = convFirst s conv b in
    if val == 0 then
      getFirst (tail s) b
    else
      val


solve :: String -> Int
solve s =
  let rs = reverse s in
  getFirst s False * 10 + getFirst rs True

main = do
  contents <- readFile "fi.txt"
  map solve (lines contents) & sum & print
