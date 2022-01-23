--B
f :: Integer -> Integer
f = \x -> x*x

g :: Integer -> Integer
g = \y -> f (f y)

h :: (t -> t) -> t -> t
h y s = y (y s)

k1 :: Integer -> Integer
k1 x = h g x

--C
s :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
s = \ f g x -> f x (g x);

k :: p1 -> p2 -> p1
k = \ x y -> x;

--(s k k)3 = 3
s' :: p -> p
s' = \ x -> x;

--F
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (y:ys) x = if x == y  then ys
                else y : rem1 ys x;
            