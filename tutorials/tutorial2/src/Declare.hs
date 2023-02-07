module Declare where


data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Power Exp Exp
         | Neg Exp
        --  deriving Show

instance Show Exp where
    show = showExp

-- TODO: Please write your answer to Question 1 below
{- Question 1: Option A, Option B, or Option C?
Answer: 
-}


e1 :: Exp
e1 = Add (Num 3) (Num 4)

e2 :: Exp
e2 = Add (Num 3) (Mult (Sub (Num 4) (Num 5)) (Num 7))

e3 :: Exp
e3 = Sub (Div (Add (Num 1) (Num 2)) (Num 3)) (Mult (Sub (Num 5) (Num 6)) (Num 8))


-- | Pretty printing
--
-- Examples:
--
-- >>> e1
-- (3 + 4)
--
-- >>> e2
-- (3 + ((4 - 5) * 7))
--
-- >>> e3
-- (((1 + 2) / 3) - ((5 - 6) * 8))
showExp :: Exp -> String
showExp (Num n)        = show n
showExp (Neg n)        = "- " ++ showExp n 
showExp (Add e1 e2)    = "(" ++ (showExp e1) ++ " + " ++ (showExp e2) ++ ")"
showExp (Sub e1 e2)    = "(" ++ (showExp e1) ++ " - " ++ (showExp e2) ++ ")"
showExp (Mult e1 e2)   = "(" ++ (showExp e1) ++ " * " ++ (showExp e2) ++ ")"
showExp (Div e1 e2)    = "(" ++ (showExp e1) ++ " / " ++ (showExp e2) ++ ")"
showExp (Power e1 e2)  = "(" ++ (showExp e1) ++ " ^ " ++ (showExp e2) ++ ")"

