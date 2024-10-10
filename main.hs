module Main where

add :: Int -> Int -> Int
add a b = a + b

sub :: Int -> Int -> Int
sub a b = a - b

data Tp = IInt Int | IString String | IBool Bool

concatenation :: Tp -> Tp -> String
concatenation (IString _str) (IInt _i) = _str ++ show _i
concatenation (IString _str) (IBool _b) = _str ++ show _b
concatenation _ _ = "Invalid types"

find :: [Int] -> Int -> Int
find [] _ = -1
find (_first:_others) search
    | _first == search = 0
    | otherwise = 1 + find _others search

main = do
    putStrLn "Hello Wolrd!"

    putStrLn (concatenation (IString "a + b: ") (IInt (add 3 4))  )
    putStrLn (concatenation (IString "a - b: ") (IInt (sub 10 2)) )
    putStrLn (concatenation (IInt 0) (IInt (sub 10 2)) )

    let list = [1, 2, 4, 6, 5, 3]
    let searchValue = 3
    let index = find list searchValue

    putStrLn (concatenation (IString "Index: ") (IInt index))
