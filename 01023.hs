import Data.List(minimumBy,sort)
import Data.Ord (comparing)
solve :: Int -> [Int] -> Int
solve x = minimumBy $ comparing $ abs.(x-)
main = do h <- getLine
          n <- getLine
          putStrLn (show (solve (read (last (words h)) :: Int) (reverse (sort (map read (words n))))))
