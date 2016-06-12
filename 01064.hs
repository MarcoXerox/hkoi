import Data.List (group, sort)
solve :: [String] -> [String]
solve xs = map head (filter isMode grp)
    where grp = group $ sort xs
          mode = maximum (map length grp)
          isMode x = length x == mode
main = do h <- getContents
          putStr (unlines (solve (tail (lines h))))
