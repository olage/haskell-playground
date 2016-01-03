module RPN (
    solve
) where 

import Data.Char

data Expr a = Plus | Minus | Prod | Const a deriving (Show)

solve :: (Num a) => [Expr a] -> Maybe a
solve = solve' []

solve' :: (Num a) => [a] -> [Expr a] -> Maybe a
solve' acc (Const x:xs)  = solve' (x:acc) xs
solve' (x:y:acc) (op:xs) = solve' (val:acc) xs
    where val = case op of Plus  -> x + y
                           Minus -> x - y
                           Prod  -> x * y
solve' [res] [] = Just res
solve' _ _ = Nothing


parse :: String -> [Expr Int]
parse "" = []
parse (x:xs)
    | x == '+'  = Plus  : parse xs
    | x == '-'  = Minus : parse xs
    | x == '*'  = Prod  : parse xs
    | x == ' '  = parse xs
    | x >= '0' && x <= '9' = case parseInt(x:xs) of
        (val, str) -> Const val : parse str
    | otherwise = []

parseInt :: String -> (Int, String)
parseInt = parseInt' 0

parseInt' :: Int -> String -> (Int, String)
parseInt' acc "" = (acc, "")
parseInt' acc (x:xs)
    | x >= '0' && x <= '9' = parseInt' (10 * acc + curDigit) xs
    | otherwise = (acc, x:xs)
        where curDigit = Data.Char.ord(x) - Data.Char.ord('0')

parse2 :: String -> [Expr Int]
parse2 = map stringToExpr . words
    where stringToExpr "+" = Plus
          stringToExpr "-" = Minus
          stringToExpr "*" = Prod
          stringToExpr num = Const (read num :: Int)
