module Interp where

import Parser
import Declare
import Data.List (nub)

type Binding = (String, Int)
type Env = [Binding]

-- var a = 3; var b = 8; var a = b, b = a; a + b
e5 :: Exp
e5 = Decl "a" (Num 3) (Decl "b" (Num 8) (Add (Var "a") (Var "b")))

-- var a = 3; var b = 8; var a = b; var b = a; a + b
e6 :: Exp
e6 = Decl "a" (Num 3) (Decl "b" (Num 8) (Decl "a" (Var "b") (Decl "b" (Var "a") (Add (Var "a") (Var "b")))))


{-
-- test cases for multiple-variable declarations

-- var x = 3, x = x + 2; x * 2  (duplicate declarations, signals an error)
e7 :: Exp
e7 = DeclareMulti [("x", (Num 3)), ("x", (Add (Var "x") (Num 2)))] (Mult (Var "x") (Num 2))


-- var a = 2, b = 7; var m = 5 * a, n = b - 1; a * n + b / m + a  (evaluate to 14)
e8 :: Exp
e8 = DeclareMulti [("a", (Num 2)), ("b", (Num 7))] (Add (DeclareMulti [("m", (Mult (Num 5) (Var "a"))), ("n", (Sub (Var "b") (Num 1)))] (Add (Mult (Var "a") (Var "n")) (Div (Var "b") (Var "m")))) (Var "a"))


-- var a = 3; var b = 8; var a = b, b = a; a + b  (evaluates to 11)
e9 :: Exp
e9 = Decl "a" (Num 3) (Decl "b" (Num 8) (DeclareMulti [("a",(Var "b")), ("b",(Var "a"))] (Add (Var "a") (Var "b"))))

-}


-- | Evaluation function
--
-- Examples:
--
-- >>> evaluate e5
-- 11
--
-- >>> evaluate e6
-- 16
evaluate :: Exp -> Int
evaluate e = eval e []  -- starts with an empty environment
  where
    eval :: Exp -> Env -> Int
    eval (Num n)    env = n
    eval (Add x y)  env = eval x env + eval y env
    eval (Sub x y)  env = eval x env - eval y env
    eval (Mult x y) env = eval x env * eval y env
    eval (Div x y)  env = eval x env `div` eval y env
    eval (Var x)    env = case lookup x env of
      Just n  -> n
      Nothing -> error ("Undeclared variable " ++ x)
    eval (Decl x e1 e2) env =
      let v    = eval e1 env
          env' =  (x, v) : env
      in
        eval e2 env'



calc :: String -> Int
calc  = evaluate . parseExpr
