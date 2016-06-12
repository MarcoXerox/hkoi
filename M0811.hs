{-# LANGUAGE ViewPatterns #-}
import Prelude hiding (splitAt, length)
import Data.Sequence
import Numeric (readDec)

-- DEPQ Implementation: O(lg^2 n) for each insertion
ins :: (Ord x, Eq x) => x -> Seq x -> Seq x
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

-- DEPQ Implementation: finds and removes 
-- No Maybes here for M0811
findMin :: Num x => Seq x -> x
findMax :: Num x => Seq x -> x
removeMin :: Seq x -> Seq x
removeMax :: Seq x -> Seq x

findMin (viewl -> EmptyL) = (-1) 
findMin (viewl -> x :< xs) = x
findMax (viewr -> EmptyR) = (-1) 
findMax (viewr -> xs :> x) = x
removeMin (viewl -> x :< xs) = xs
removeMax (viewr -> xs :> x) = xs

-- M0811 Implementation
wts q x = case x of
            2 -> [findMin q]
            3 -> [findMax q]
            _ -> []
parse q [] = []
parse q [x] = wts q x
parse q (x:xs@(y:s)) = 
    case x of
        1 -> parse (ins y q) s
        2 -> findMin q : parse q xs
        3 -> findMax q : parse q xs
        4 -> parse (removeMin q) xs
        5 -> parse (removeMax q) xs

fastread s = case readDec s of [(n, "")] -> n
main = interact $ unlines.(map show).(parse empty).map fastread.tail.words
