module Interp where

import Parser
import Declare
import Prelude hiding (Either(..))


-- | Evaluation function
--
-- Examples:
--
-- >>> evaluate (Neg (Num 3))
-- -3
--
-- >>> evaluate (Power (Num 2) (Num 3))
-- 8
evaluate :: Exp -> Int
evaluate (Num n)       = n
evaluate (Neg e1)      = - (evaluate e1)
evaluate (Add e1 e2)   = evaluate e1 + evaluate e2
evaluate (Sub e1 e2)   = evaluate e1 - evaluate e2
evaluate (Mult e1 e2)  = evaluate e1 * evaluate e2
evaluate (Div e1 e2)   = evaluate e1 `div` evaluate e2
evaluate (Power e1 e2) = evaluate e1 ^ evaluate e2


-- | A simple calculator
--
-- Examples:
--
-- >>> calc "1 + 8 * 2"
-- 17
--
-- >>> calc "2 * (8 + -6) ^ 3"
-- 16
--
-- >>> calc "3 * 4 - 1"
-- 11
calc :: String -> Int
calc s = evaluate $ parseExpr s


-- | Error handling

data Either a b = Left a | Right b deriving Show

safeHead :: [a] -> Either String a
safeHead []    = Left "can't access the head of an empty list"
safeHead (x:_) = Right x


-- | Evaluation function, revisited
--
-- Examples:
--
-- >>> evaluate2 (Add (Sub (Num 3) (Num 2)) (Mult (Num 2) (Num 3)))
-- Right 7
--
-- >>> evaluate2 (Div (Num 2) (Num 0))
-- Left "Divided by zero: 0"
--
-- >>> evaluate2 (Power (Num 2) (Num (-3)))
-- Left "To the power of a negative number: -3"
evaluate2 :: Exp -> Either String Int
evaluate2 (Num n)     = Right n
evaluate2 (Neg n)     = 
    case evaluate2 n of
        Left msg -> Left msg
        Right n  -> Right (-n)
evaluate2 (Add e1 e2) = 
    case evaluate2 e1 of
        Left msg -> Left msg
        Right n  -> case evaluate2 e2 of
                        Left msg -> Left msg
                        Right m  -> Right (n + m)
evaluate2 (Sub e1 e2) = 
    case evaluate2 e1 of
        Left msg -> Left msg
        Right n  -> case evaluate2 e2 of
                        Left msg -> Left msg
                        Right m  -> Right (n - m)
evaluate2 (Mult e1 e2) = 
    case evaluate2 e1 of
        Left msg -> Left msg
        Right n  -> case evaluate2 e2 of
                        Left msg -> Left msg
                        Right m  -> Right (n * m)
evaluate2 (Div e1 e2) = 
    case evaluate2 e1 of
        Left msg -> Left msg
        Right n  -> case evaluate2 e2 of
                        Left msg -> Left msg
                        Right 0  -> Left $ "Divided by zero: " ++ show e2
                        Right m  -> Right (n `div` m)
evaluate2 (Power e1 e2) = 
    case evaluate2 e1 of
        Left msg -> Left msg
        Right n  -> case evaluate2 e2 of
                        Left msg -> Left msg
                        Right m  -> if m < 0 then Left $ "To the power of a negative number: " ++ show e2
                                    else Right (n ^ m)


calc2 :: String -> Either String Int
calc2 s = evaluate2 $ parseExpr s
