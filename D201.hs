fib :: Int -> Int
fib n = fibs !! n
    where fibs = 0:1:zipWith (+) fibs (tail fibs)
main = do h <- getLine
          putStrLn (show (fib (read h)))
