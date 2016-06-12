j100 :: Int -> [[Int]]
j140 :: Int -> [[Int]]
j150 :: Int -> [[Int]]
s150 :: Int -> [[Int]]
j100 n = zipWith (\x y -> [x+1..y]) tr (tail tr) 
    where tr = scanl (+) 0 $ [1..n] ++ [n-1,n-2..1]
j140 n = [take n (iterate (j+) (j*j)) | j <- [1..n]]
j150 1 = [[1]]
j150 n = [map ((4*(n-1)*x)+) (f n) | x <- [0..(n-1)]]
    where f n = [1,5..4*n-3]
s150 x = [reverse $ take j [1, j..] | j <- [1..x]]

-- substitute solve for any function names above
main = putStr . unlines . map unwords . map (map show) . solve . read =<< getLine

s140' :: Int -> [[Int]]
s140' n = zipWith form [1..n] (xs n)
    where form x y = map ((y+).(x*)) [0..n-1] 
xs n = [k+n*(sum [0..k-1]) | k <- [1..n]]
s140 = do h <- getLine
          putStr $ unlines $ reverse $ map (unwords . map show . reverse) (s140' (read h))
