import Prelude hiding (pi);

--A

data Ast = V Int | P Ast Ast | M Ast Ast deriving (Show);

eval :: Ast -> Int
eval (V x) = x;
eval (P x y) = eval x + eval y;
eval (M x y) = eval x * eval y;

--B

evalb :: Ast -> Bool
evalb (V x) = odd x;
evalb (P x y) = evalb x || evalb y;
evalb (M x y) = evalb x && evalb y;

--C

vi :: Int -> Int
vi x = x;

pi :: Int -> Int -> Int
pi x y = x + y;

mi :: Int -> Int -> Int
mi x y = x * y;

vb :: Int -> Bool
vb = odd

pb :: Bool -> Bool -> Bool
pb x y = x || y;

mb :: Bool -> Bool -> Bool
mb x y = x && y;

vStr :: Show a => a -> String
vStr x = show x; 

pStr :: [Char] -> [Char] -> [Char]
pStr x y = x ++ "+" ++ y;

mStr :: [Char] -> [Char] -> [Char]
mStr x y = x ++ "*" ++ y;

ev :: Ast -> (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> a
ev (V t) v p m = v t;
ev (P t t') v p m = p (ev t v p m) (ev t' v p m);
ev (M t t') v p m = m (ev t v p m) (ev t' v p m);

--D

data Expr = Val Int | Add Expr Expr;

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x;
folde f g (Add x y) = g(folde f g x) (folde f g y);