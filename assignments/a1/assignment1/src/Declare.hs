module Declare where


data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Power Exp Exp
         | Neg Exp
         | Fact Exp
         | Mod Exp Exp

instance Show Exp where
  show = showExp


e1 :: Exp
e1 = Add (Num 3) (Num 4)

e2 :: Exp
e2 = Add (Num 3) (Mult (Sub (Num 4) (Num 5)) (Num 7))

e3 :: Exp
e3 = Sub (Div (Add (Num 1) (Num 2)) (Num 3)) (Mult (Sub (Num 5) (Num 6)) (Num 8))

e4 :: Exp
e4 = Mod (Fact (Num 3)) (Num 4)

e5 :: Exp
e5 = Add (Fact (Num 0)) (Mod (Num 6) (Num 3))

e :: Exp
e = Div (Add (Num 5) (Power (Num 5) (Num 2))) (Sub (Num 6) (Mult (Num 3) (Num 1)))

-- 
-- TODO: please write down the evaluation result of `e` in the comment below
-- 
-- Answer: 10
-- 



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
--
-- >>> e4
-- ((3 ! ) % 4)
--
-- >>> e5
-- ((0 ! ) + (6 % 3))
showExp :: Exp -> String
showExp (Num n)           = show n
showExp (Add exp1 exp2)   = "(" ++ showExp exp1 ++ " + " ++ showExp exp2 ++ ")"
showExp (Sub exp1 exp2)   = "(" ++ showExp exp1 ++ " - " ++ showExp exp2 ++ ")"
showExp (Mult exp1 exp2)  = "(" ++ showExp exp1 ++ " * " ++ showExp exp2 ++ ")"
showExp (Div exp1 exp2)   = "(" ++ showExp exp1 ++ " / " ++ showExp exp2 ++ ")"
showExp (Power exp1 exp2) = "(" ++ showExp exp1 ++ " ^ " ++ showExp exp2 ++ ")"
showExp (Neg exp)         = "(" ++ " - " ++ showExp exp ++ ")"
showExp (Fact exp)        = "(" ++ showExp exp ++ " ! " ++ ")"
showExp (Mod exp1 exp2)   = "(" ++ showExp exp1 ++ " % " ++ showExp exp2 ++ ")"
