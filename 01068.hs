import Data.List(sortBy)
pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:y:xs) = (x,y) : pairs xs
convert (x,y) = (show x) ++ " " ++ (show y)
cmp (x1,y1) (x2,y2) = compare (x1^2+y1^2) (x2^2+y2^2)
solve ps = map convert $ sortBy cmp $ pairs ps
main = do interact $ unlines.solve.map read.tail.words
