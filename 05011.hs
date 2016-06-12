import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Array
matI :: Int -> [[Int]]
matI n = [[fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]
diverge = 2^30
mCoins :: [Int] -> Int -> [Int]
mCoins faces p = nCoins p
    where ln = length faces
          matId = listArray (0,ln-1) (matI ln)
          nCoins 0 = replicate ln 0
          nCoins price = zipWith (+) (fst m') (matId ! (snd m'))
            where m' = minimumBy (comparing (\x -> sum  $ fst x)) (zip m [0..(ln-1)]) 
                  m = map fetch faces
                  fetch num = if (price < num) then (replicate ln diverge) else nCoinsArray ! (price - num) 
          nCoinsArray = listArray (0,p) [nCoins i | i <- [0..p]] 

solve xs = map show ((sum m) : m)
    where m = mCoins (tail (tail xs)) (head xs)
main = interact $ unlines.solve.map read.words
