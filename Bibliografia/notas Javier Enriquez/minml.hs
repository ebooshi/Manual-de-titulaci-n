module MinML where

data Type = Natural | Boolean | Arrow Type Type

data Expr = Var String
           | Num Int
           | Boo Bool
           | Add Expr Expr
           | Prod Expr Expr
           | If Expr Expr Expr
           | Let String Type Expr Expr
           | Letrec String Type Expr Expr
           | Fun String Type Expr
           | App Expr Expr
           | Fix Type String Expr
           deriving(Show,Eq)

data Value = N Int | B Bool | F String Type Expr

v2e :: Value -> Expr 
v2e (N n)     = Num n
v2e (B b)     = Boo n
v2e (F s t b) = Fun s t b

sust :: Expr -> String -> Value -> Expr
sust e i v = sust' e i (v2e v)

sust' :: Expr -> String -> Expr -> Expr
sust' e@(Var x) i v | x == i = v | otherwise e
sust' e@(Num n) _ _ = e
sust' e@(Boo b) _ _ = e
sust' (Add a b ) i v  = Add (sust a i v) (sust a i v)
sust' (Prod a b ) i v = Prod (sust a i v) (sust a i v)
sust' (If g t e) i v = IF (sust g i v) (sust t i v) (sust e i v)
sust' e@(Let s t a b) i v 
 | s `notElem` (fv v) && i != s = Let s t (sust a i v) (sust b i v)
 | otherwise = e
sust' e@(Letrec s t a b) i v 
 | s `notElem` (fv v) && i != s = Letrec s t (sust a i v) (sust b i v)
 | otherwise = e
sust' e@(Fun s t b) i v 
 | s `notElem` (fv v) && i != s = Fun s t (sust b i v)
 | otherwise = e
sust' (Fix t s f) i v = Fix t s (sust f i v)
sust' (App f p) = App (sust f i v) (sust p i v)

fv :: Expr -> [String]
fv (Var x) = [x]
fv (Add a b) = fv a ++ fv b
fv (Prod a b) = fv a ++ fv b
fv (If a b c) = fv a ++ fv b ++ fv c
fv (Fun x _ a) = filter (/= x) (fv a)
fv (Let x _ a b) = (fv a) ++ $$ filter (/= x) (fv b)
fv (LetRec x _ a b) = filter (/= x) (fv a) ++ filter (/= x) (fv b)
fv (App a b) = fv a ++ fv b
fv (Fix x a) = fv (Fun x a)
fv e = []

eval :: Expr -> Value
eval (Var _)   = error "Free variable"
eval (Num n)   = N n
eval (Boo b)   = B b
eval (Add a b) =
	let (N n) = eval a
	    (N m) = eval b in N $ n+m
eval (Prod a b) =
	let (N n) = eval a
	    (N m) = eval b in N $ n*m
eval (If g t e) =
	let (B b) = eval g in if b then eval t else eval e
eval (Let i _ v b) = 
	let v' = eval v in eval $ sust b i v'
eval (Letrec i t v b) = eval $ sust' b i (Fix t i v)
eval (Fun p t b) = F p t b
eval (App f p) = 
	let (F x _ b) = eval f
	    v = eval p in eval $ sust b x v
eval (Fix f t a) = eval $ sust' a f (Fix f t a)
