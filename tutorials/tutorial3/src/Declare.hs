module Declare where
import Data.List

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         | Var String           -- new
         | Decl String Exp Exp  -- new
         | DeclareMulti [(String, Exp)] Exp


e1 :: Exp
e1 = Decl "x" (Num 3) (Mult (Var "x") (Num 3))

e2 :: Exp
e2 = Decl "x" (Add (Num 3) (Num 4)) (Var "x")

e3 :: Exp
e3 = Add (Var "x") (Mult (Num 4) (Num 5))

e4 :: Exp
e4 = Decl "y" e3 (Div (Var "x") (Var "y"))


instance Show Exp where
  show = showExp


-- | Pretty printer
--
-- Examples:
--
-- >>> e1
-- var x = 3; (x * 3)
--
-- >>> e2
-- var x = (3 + 4); x
--
-- >>> e3
-- (x + (4 * 5))
--
-- >>> e4
-- var y = (x + (4 * 5)); (x / y)
showExp :: Exp -> String
showExp (Num n)        = show n
showExp (Add e1 e2)    = "(" ++ (showExp e1) ++ " + " ++ (showExp e2) ++ ")"
showExp (Sub e1 e2)    = "(" ++ (showExp e1) ++ " - " ++ (showExp e2) ++ ")"
showExp (Mult e1 e2)   = "(" ++ (showExp e1) ++ " * " ++ (showExp e2) ++ ")"
showExp (Div e1 e2)    = "(" ++ (showExp e1) ++ " / " ++ (showExp e2) ++ ")"
showExp (Var x)        = x
showExp (Decl x exp1 exp2) = "var " ++ x ++ " = " ++ (showExp exp1) ++ "; " ++ (showExp exp2)
showExp (DeclareMulti bs exp) = "var " ++ (intercalate "," (map (\(x,e) -> x ++ " = " ++ (showExp e)) bs)) ++ (showExp exp)

-- | Renaming function
--
-- Examples:
--
-- >>> rename "x" "i" e3
-- (i + (4 * 5))
--
-- >>> rename "x" "i" e1
-- var x = 3; (x * 3)
--
-- >>> rename "x" "i" e4
-- var y = (i + (4 * 5)); (i / y)
rename :: String -> String -> Exp -> Exp
rename old new e = re e
  where
    re (Num n)        = Num n
    re (Add  x y)     = Add  (re x) (re y)
    re (Sub  x y)     = Sub  (re x) (re y)
    re (Mult x y)     = Mult (re x) (re y)
    re (Div x y)      = Div  (re x) (re y)
    re (Var x)        = if x == old then (Var new) else (Var old)
    re (Decl x e1 e2) = 
      if x == old
        then Decl x (re e1) (e2)
        else Decl x (re e1) (re e2)