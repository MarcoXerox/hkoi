{-# LANGUAGE ViewPatterns #-}
import Prelude hiding (splitAt, length)
import Data.Sequence
parse n (viewl -> x :< (viewl -> y:<xs)) = parse (n + x + y) (ins (x + y) xs)
parse n (viewl -> x :< empty) = n

ins n s@(viewl -> x :< (viewl -> y:<xs))
    | leftR > n = (ins n lefts) >< rights
    | rightL < n = lefts >< (ins n rights)
    | otherwise = lefts >< (singleton n) >< rights
    where (lefts, rights) = splitAt (div (length s) 2) s
          (_ :> leftR) = viewr lefts
          (rightL :< _) = viewl rights

-- if zero element
ins n (viewl -> EmptyL) = singleton n
-- if one element only
ins n s@(viewl -> x :< xs) = if x <= n then s >< p else p >< s where p = singleton n

solve xs = parse 0 $ sort $ fromList xs
main = do interact $ show.solve.map read.tail.words
          putStr "\n"
