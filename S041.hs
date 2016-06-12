import Data.Char
split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:s) = (x:xs, y:ys) where (xs,ys) = split s

solve :: String -> Int
solve s = if (d == 10) then -1 else d
    where (p,q) = split $ map digitToInt s
          m = mod ((sum p) - (sum q)) 11
          e = odd (length s)
          d = if e then m else mod (11 - m) 11

main = do _ <- getLine
          a <- getLine
          putStrLn (show (solve a))
