view lst = maximum $ scanl (+) 0 lst
solve :: [Int] -> Int
solve [] = 0
solve [x] = x
solve chain@(_:_:_) = maximum [(solve f), (solve s), mid]
    where (f, s) = splitAt (div (length chain) 2) chain
          mid = view (reverse f) + view s

main :: IO()
main = do interact $ show.solve.map read.tail.words 
          putStr "\n"
