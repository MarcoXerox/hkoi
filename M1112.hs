import qualified Data.Map as M
import Data.Char (digitToInt)
zeros = M.fromList [(j, 0) | j <- ['0'..'9']]
data Exp x = Node Int [Exp x] | Leaf x deriving Show
expand :: Exp Char -> M.Map Char Int 
expand (Leaf a) = M.singleton a 1
expand (Node cnt list) = M.map (cnt *) $ foldr (M.unionWith (+)) zeros $ map expand list 
parse' :: String -> [Exp Char]
parse' [] = []
parse' [_] = []
parse' (x:xs@(r:rs))
    | r == '(' = Node (digitToInt x) (parse' tokens) : parse' rems
    | otherwise = Leaf x : parse' xs
    where parseCollect 0 ps qs = (ps, qs)
          parseCollect n ps (q:qs) = parseCollect (n + left - right) (ps ++ [q]) qs
            where (left, right) = (fromEnum (q == '('), fromEnum (q == ')'))
          (tokens, rems) = parseCollect 1 [] rs
parse s = head $ parse' $ "1(" ++ s ++ ")"
main = putStrLn.unwords.(map (show.snd)).M.toList.expand.parse =<< getLine
