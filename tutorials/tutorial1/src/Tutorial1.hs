module Tutorial1 where

import Data.Char (ord)
import Prelude hiding (Maybe(..))



absolute :: Int -> Int
absolute x = if x < 0 then -x else x

nested_if :: Int -> Int
nested_if x = if absolute x <= 10 then x
else error "Only numbers between [-10,10] allowed"

nested_if' :: Int -> Int
nested_if' x = if (if x < 0 then -x else x) <= 10 then x
else error "Only numbers between [-10,10] allowed"

{-

Write your answers of Q1 to Q3 below

Q1: Yes, both are valid programs
Q2: Nope, this would not be valid in Java
Q3: Yes, Polymorphism exists in other languages. For example, in Python the '+' operator
performs different functionalities if the parameters are integers vs strings. It's referred to as overloading in Python

-}

-- | The tail of a list
--
-- Examples:
--
-- >>> tl [1,2,3]
-- [2,3]
--
-- >>> tl [1]
-- []
tl :: [a] -> [a]
tl []     = error "Cannot get tail of empty list"
tl (x:xs) = xs


-- | Q5. Factorial function
--
-- Examples:
--
-- >>> factorial 3
-- 6
--
-- >>> factorial 0
-- 1
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)


-- | Q6. Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fibonacci 10
-- 55
--
-- >>> fibonacci 5
-- 5
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci(n-2)


-- | Q7.
-- >>> mapList absolute [4,-5,9,-7]
-- [4,5,9,7]
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = [f x] ++ (mapList f xs)



-- | Q8
-- >>> ascii "abcds"
-- [97,98,99,100,115]
ascii :: [Char] -> [Int]
ascii [] = []
ascii (x:xs) = [ord x] ++ (ascii xs)



-- | Q9
-- >>> filterList even [1,2,3,4,5]
-- [2,4]
filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList f (x:xs) = if f x then [x] ++ (filterList f xs) else (filterList f xs)



-- | Q10
-- >>> zipList [1,2,3] ['a', 'b', 'c']
-- [(1,'a'),(2,'b'),(3,'c')]
zipList :: [a] -> [b] -> [(a,b)]
zipList _ [] = []
zipList [] _ = []
zipList (n:ns) (x:xs) = [(n,x)] ++ (zipList ns xs)



-- | Q11
-- >>> zipSum [1,2,3] [4,5,6]
-- [5,7,9]
zipSum :: [Int] -> [Int] -> [Int]
zipSum xs ys = map (\(x, y) -> x+y) (zipList xs ys)



data Maybe a = Nothing | Just a deriving Show

-- | Q12
-- >>> safeHead []
-- Nothing
--
-- >>> safeHead [1,2,3]
-- Just 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x


-- | Q13
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1,2]
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing: xs) = catMaybes xs
catMaybes (Just x: xs) = [x] ++ (catMaybes xs)
