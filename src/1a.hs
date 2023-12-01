import Data.Char (isDigit)
import Data.Function ( (&) )

solve :: String -> Int
solve s =
  let sf = filter isDigit s
  in read [head sf, last sf] :: Int

main = do
  contents <- readFile "fi.txt"
  map solve (lines contents) & sum & print
