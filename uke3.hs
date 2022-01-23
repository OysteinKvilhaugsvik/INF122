

-- B. Hva er typene til disse funksjonene?
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (b -> b) -> b -> b
twice f x = f (f x)

-- C. angi typene til følgende uttrykk.

u1 :: Num a => a -> a
u1 = (+) 2

u2 :: Num a => a -> a
u2 = (+2)

u3 :: Num a => a -> a
u3 = (2+)

u4 :: [(Bool, [[Char]])]
u4 = [(True, []), (False, [['a']])]

u5 :: Int -> [a] -> a
u5 = \x y -> y !! x

--Feil. 
--u6 = [take, drop, \x y -> (y !! x)]

u7 :: [(Int -> [a] -> [a])]
u7 = [take, drop, \x y -> [y !! x]]


-- D. Angi typen til hver foo og si hvilke som er ekvivalente.

foo1 :: a -> b -> (a, b)
foo1 x y = (x, y)

foo2 :: a -> b -> (a, b)
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a, b)
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a, b)
foo4 = \x -> \y -> (x, y)

foo5 :: a -> b -> (b, a)
foo5 = \x -> \y -> (y, x)

foo6 :: a -> b -> (a, b)
foo6 = \y -> \x -> (y, x)



-- E. Programmer vilkårlige (gjerne enklest mulige) funksjoner med følgende typer:

f1 :: a -> (a, a)
f1 a = (a, a)

f2 :: (a, b) -> a
f2 (a, _) = a

f3 :: (a, b) -> b
f3 (_, b) = b

f4 :: a -> b -> a
f4 a _ = a

f5 :: a -> b -> b
f5 _ b = b



-- F. Programmer to funksjoner f og g med egenskapen at for alle x og alle y
--  er f x y == g (x, y).

f :: Int -> Int -> Int
f x y = x + y + y

g :: (Int, Int) -> Int
g (x, y) = f x y