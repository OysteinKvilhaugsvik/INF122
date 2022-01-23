--D
foo1 :: a -> b -> (a, b)
foo1 x y = (x, y);

foo2 :: a -> b -> (a, b)
foo2 x = \y -> (x, y);

foo3 :: a -> b -> (a, b)
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a, b)
foo4 = \x -> \y -> (x, y)

foo5 :: b -> a -> (a, b)
foo5 = \x -> \y -> (y,x)
--Alle funksjonene er ekvivalente utenom 5

foo6 :: a -> b -> (a, b)
foo6 = \y -> \x -> (y,x)


--e 
f1 :: b -> (b, b)
f1 a = (a,a);

f2 :: (a, b) -> a
f2 (a,b) = a;

f3 :: (a, b) -> b
f3 (a,b) = b;

f4 :: a -> b -> a
f4 a b = a;

f5 :: a -> b -> b
f5 a b = b;


--f
f :: Int -> Int -> Int
f a b = a;
g :: (Int, Int) -> Int
g (a, b) = a;
--f 3 4 = 3
--g (3, 4) = 3


import Data.Char (toLower, isAlpha)

--Løsningsforslag skrevet av Steinar

-- E Programmer funksjonen flett :: [Int] -> [Int] -> [Int], 
--  som fletter to sorterte lister til én lang sortert liste.
--  eksempel: flett [1,3,5] [2,4,6,7,8] = [1,2,3,4,5,6,7,8]
flett :: [Int] -> [Int] -> [Int]
flett [] ys = ys
flett xs [] = xs
flett (x:xs) (y:ys) = if x < y then x : flett xs (y:ys)
                      else y : flett (x:xs) ys

flett2 :: [Int] -> [Int] -> [Int]
flett2 [] ys = ys
flett2 xs [] = xs
flett2 (x:xs) (y:ys) | x < y = x : flett xs (y:ys)
                     | otherwise = y : flett (x:xs) ys


-- F Programmer funksjonen ele :: Int -> [a] -> a,
--  som gjør det samme som (!!) bare at listens elementer numereres fra 1 opptil listens lengde

ele :: Int -> [a] -> a
ele n xs = xs !! (n-1)


ele2 :: Int -> [a] -> a
ele2 _ [] = error "Indeksfeil"
ele2 1 (x:_) = x
ele2 n (_:xs) = ele2 (n-1) xs



-- G. Programmer funksjonen addobb :: [Int] -> [Int]
--  som tar en liste k, og returner listen k etterfulgt av (dobb k).
dobb :: [Int] -> [Int]
dobb [] = []
dobb (x:xs) = 2 * x : dobb xs

addobb :: [Int] -> [Int]
addobb xs = xs ++ dobb xs


-- H Programmer funksjonen pali :: [a] -> Bool,
--  som returner True/False om listen er et palindrom eller ikke.
pali :: Eq a => [a] -> Bool
pali xs = xs == reverse xs



-- En bedre (?) versjon som filtrerer ut mellomrom og komma osv, 
--  og som ikke bryr seg om store vs små bokstaver.
--  Dette var altså ikke oppgaven, men bare på gøy.
--  toLower og isAlpha kommer fra Data.Char.
truePalindrome :: String -> Bool
truePalindrome xs = pali (converter xs)

converter :: String -> String
converter "" = ""
converter (x:xs) | isAlpha x = toLower x : converter xs
                 | otherwise = converter xs


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

-- F. Programmer to funksjoner f og g med egenskapen at for alle x og alle y
--  er f x y == g (x, y).

f :: Int -> Int -> Int
f x y = x + y + y

g :: (Int, Int) -> Int
g (x, y) = f x y

--Løsningsforslag laget av Steinar

-- C. Gitt følgende definisjoner, skriv en enklest mulig definsjon av resultatet (s k k)

s = \f g x -> f x (g x)
k = \x y -> x


{-
  s k k
= (\f g x -> f x (g x)) k k
= (\g x -> k x (g x)) k
= (\x -> k x (k x))
= \x -> x
= id        (siden den gjør det samme som den innebygde funksjonen id)
-}


--- F. Programmer funksjonen rem1 som fjerner det første elementet fra en liste som er likt det andre argumentet.

rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) c | x == c = xs 
              | otherwise = x : rem1 xs c


module Oblig1 where

dictionary :: [(String, [String])]
dictionary = [
        ("bb",["Big Brother"]),
        ("dep",["department"]),
        ("sec", ["Sector"]),
        ("doubleplusgood",["excellent", "fabulous", "fantastic", "best"]),
        ("doubleplusungood", ["terrible", "horrible", "worst"]),
        ("Ingsoc", ["English Socialism"]),
        ("joycamp", ["labour camp"]),
        ("Oldspeak", ["Standard English", "English"]),
        ("oldthink", ["objectivity", "rationalism", "democracy"]),
        ("thinkpol", ["The Thought Police"]),
        ("prolefeed", ["Popular culture", "pop-culture"]),
        ("crimethink", ["liberty", "equality", "privacy", "thoughtcrime"]),
        ("fullwise", ["fully", "completely", "totally"]),
        ("goodthink", ["political orthodoxy", "politically orthodox thought", "orthodox thought"]),
        ("goodwise", ["well"]),
        ("ownlife", ["anti-social tendency", "solitude", "individualism"]),
        ("plusgood", ["very good", "great"]),
        ("plusungood", ["very bad"]),
        ("misprint", ["error", "misprediction"]),
        ("Miniluv", ["The Ministry of Love"]),
        ("Minipax", ["The Ministry of Peace"]),
        ("Minitrue", ["The Ministry of Truth"]),
        ("Miniplenty", ["The Ministry of Plenty"]),
        ("bellyfeel", ["blind, enthusiastic acceptance"]),
        ("doublethink", ["believing two contradictory ideas"]),
        ("duckspeak", ["vocal support of political orthodoxies"]),
        ("un", ["not"]),
        ("peace", ["war"]),
        ("strength", ["ignorance"]),
        -- The next line contains a list of forbidden words that don't have a translation to Newspeak, these should be replaced with '*'s
        ("",["freedom", "revolution", "fun", "diary", "surveillance", "Great Britain", "Winston Smith", "Julia"])
        ]


-- Oppgave 1 ----------------------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix p s = take (length p) s == p




isPrefix2 :: String -> String -> Bool
isPrefix2 [] _ = True
isPrefix2 _ [] = False
isPrefix2 (x:xs) (y:ys) = x == y && isPrefix2 xs ys


-- Oppgave 2 ----------------------------------------------------
locate :: String -> String -> [(Int,Int)]
locate p s = locateHelper p s 0

locateHelper :: String -> String -> Int -> [(Int, Int)]
locateHelper _ [] _ = []
locateHelper p s i | isPrefix p s = (i, i + length p) : locateHelper p (drop (length p) s) (i+length p)
                   | otherwise    = locateHelper p (tail s) (i+1)




locate2 :: String -> String -> [(Int, Int)]
locate2 p s = [ (i, i+length p) | i<-[0..length s], isPrefix p (drop i s) ]



-- Oppgave 3 ----------------------------------------------------
translate :: String -> String 
translate s | elem s illegals = replicate (length s) '*'
            | otherwise       = translateHelper s dictionary

translateHelper :: String -> [(String, [String])] -> String
translateHelper s [] = ""
translateHelper s ((new, olds):dct) | elem s olds = new
                                    | otherwise   = translateHelper s dct


illegals :: [String]
illegals = snd (last dictionary)


-- Oppgave 4 ----------------------------------------------------
replaceHelper :: [(Int,Int)] -> String -> String
replaceHelper [] s = s
replaceHelper ((a, b):ts) s = replaceHelper ts (before ++ translation ++ after)
    where before = take a s
          translation = translate (take (b-a) (drop a s))
          after  = drop b s

replace :: [(Int,Int)] -> String -> String
replace ts s = replaceHelper (reverse ts) s




replace2 :: [(Int, Int)] -> String -> String
replace2 ts s = foldr (\(a, b) s -> take a s ++ translate (take (b-a) (drop a s)) ++ drop b s) s ts


-- Oppgave 5 ----------------------------------------------------
toNewspeak :: String -> String
toNewspeak s = toNewspeakHelper s oldwords

toNewspeakHelper :: String -> [String] -> String
toNewspeakHelper s [] = s
toNewspeakHelper s (old:olds) = toNewspeakHelper (replace (locate old s) s) olds

oldwords :: [String]
oldwords = concatMap snd dictionary



toNewspeak2 :: String -> String
toNewspeak2 s = foldl (\s old -> replace (locate old s) s) s oldwords

-- Oppgave 6 ----------------------------------------------------
analytics :: String -> String -> Int 
analytics old new = (100 * sum (zipWith (\a b -> max 0 (a-b)) (counter old old) (counter new old))) /~ length old

(/~) :: Int -> Int -> Int
x /~ y = round (fromIntegral x / fromIntegral y)

--fra Data.List
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/=x) xs)

counter :: String -> String -> [Int]
counter str old = map (\c -> length (filter (c==) str)) (nub old)


-- Oppgave 7
wrapper :: String -> (String, Int)
wrapper old = (toNewspeak old, analytics old (toNewspeak old))



{-
Programmer funksjon 
remg :: [a] -> (a -> Bool) -> [a]
som fjerner fra listen i første argumentet det første element som opfyller 
egenskapen gitt i det andre argumentet, f.eks.:
  remg "abab" (< 'b')   = "bab"
  remg "abab" (== 'b') = "aab"
  remg "abab" (> 'b')   = "abab"
  remg [1,2,3,4] even  = [1,3,4]

-}

remg :: [a] -> (a -> Bool) -> [a]
remg [] _ = []
remg (x:xs) f | f x = xs
              | otherwise = x : remg xs f

rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) c | c == x = xs
              | otherwise = x : rem1 xs c

rem12 :: Eq a => [a] -> a -> [a]
rem12 xs c = remg xs (c==)


{-
Definer en funksjon altMap :: (a -> b) -> (a -> b) -> [a] -> [b] 
 som vekselvis anvender to funksjoner på alle elementene i en liste.
-}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = [f x]
altMap f g (x:y:xs) = f x : g y : altMap f g xs

altMap2 :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap2 _ _ [] = []
altMap2 f g (x:xs) = f x : altMap2 g f xs

-- O(n^2), ikke gjør dette i produksjon, kun når du skal flekse
altMap3 :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap3 f g xs = r
    where (r, _, _) = foldl (\(l, f, g) x -> (l++[f x], g, f)) ([], f, g) xs



import Prelude hiding (pi)

data Ast = V Int | P Ast Ast | M Ast Ast 

{-
Skriv en funksjon
    eval :: Ast -> Int
som evaluerer Ast-en som et matematisk uttrykk, der P er pluss, M er ganging.
-}

eval :: Ast -> Int
eval (V a) = a
eval (P a b) = eval a + eval b
eval (M a b) = eval a * eval b

eval2 :: Ast -> Int
eval2 t = ev t vi pi mi

vi = id
pi = (+)
mi = (*)

{-
Skriv en funksjon
    evalb :: Ast -> Bool
som evaluerer Ast-en som et boolsk uttrykk, der (V n) er True om b er et oddetall, usant ellers.
P blir or, og M blir and.
-}
evalb :: Ast -> Bool 
evalb (V a) = odd a
evalb (P a b) = evalb a || evalb b
evalb (M a b) = evalb a && evalb b

evalb2 :: Ast -> Bool
evalb2 t = ev t vb pb mb

vb = odd
pb = (||)
mb = (&&)

{-
Skriv en mer generell versjon av eval og evalb, som bruker parametrene til å evaluere Ast-en, istedet for å hardkode +, ||, etc.
-}

ev :: Ast -> (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> a 
ev (V a) v p m = v a
ev (P a b) v p m = p (ev a v p m) (ev b v p m)
ev (M a b) v p m = m (ev a v p m) (ev b v p m)


evals :: Ast -> String
evals t = ev t vStr pStr mStr

vStr = show
pStr a b = "(" ++ a ++ "+" ++ b ++ ")"
mStr a b = "(" ++ a ++ "*" ++ b ++ ")"




-- Eksempel på aksiomatiske tester
testI :: Ast -> Bool
testI ast = ev ast vi pi mi == eval ast

testV :: Ast -> Bool
testV ast = ev ast vb pb mb == evalb ast




data Expr = Val Int | Add Expr Expr
{-
define a higher-order function
    folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
such that folde f g replaces each Val constructor in an expression by the
function f, and each Add constructorby the function g.
-}


folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val a) = f a
folde f g (Add a b) = folde f g a `g` folde f g b



{-
Using folde, define a function 
    eval :: Expr -> Int 
that evaluates an ex-pressionvto an integer value,
and a function 
    size :: Expr -> Int 
that calculates the number of values in an expression.
-}

evalF :: Expr -> Int
evalF = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)



-- Eksempler på do, <- og >>=. f og g er ekvivalente.
f :: IO String
f = do
    s <- getLine
    return (s ++ "!")

g :: IO String
g = getLine >>= \s -> return (s ++ "!")


--Eksempel på >>. Den bare evaluerer hver side etter hverandre uten å bry seg om resultatene.
h :: IO ()
h = putStrLn "Hallo!" >> putStrLn "Ha det bra!"




trekant :: Int -> IO ()
trekant n = mapM_ (\n -> putStrLn (concat $ replicate n "* ")) [1..n]

trekant2 :: Int -> IO ()
trekant2 n = mapM_ (\m -> putStrLn (replicate (n-m) ' ' ++ (concat (replicate m "* ")))) [1..n]

trekanter :: Int -> Int -> Int -> IO ()
trekanter n1 n2 n3 = mapM_ (\m -> putStrLn $ treeline n1 m ++ treeline n2 m ++ treeline n3 m) [1..height]
    where height       = maximum [n1, n2, n3]
          treeline n m = line (m + n - height) (n*2+1)

line :: Int -> Int -> String
line n_stars total | n_stars <= 0 = replicate total ' '
                   | otherwise    = spaces ++ stars ++ spaces
    where stars = unwords $ replicate n_stars "*"
          spaces = replicate ((total - length stars) `div` 2) ' '

data FileOrFolder = File Int | Folder [FileOrFolder]

prettyPrint :: FileOrFolder -> IO ()
prettyPrint f = prettyPrintHelper 0 f
    where prettyPrintHelper n_spaces (File a)    = putStrLn (replicate n_spaces ' ' ++ "File " ++ show a)
          prettyPrintHelper n_spaces (Folder fs) = do
              putStrLn (replicate n_spaces ' ' ++ "Folder " ++ show (length fs))
              mapM_ (prettyPrintHelper (n_spaces+2)) fs

trav :: (a -> FileOrFolder -> IO ()) -> (a -> FileOrFolder -> IO ()) -> (a -> a) -> a -> FileOrFolder -> IO () 
trav file _      _      s (File a)    = file s (File a)
trav file folder update s (Folder fs) = folder s (Folder fs) >> mapM_ (trav file folder update (update s)) fs

prettyPrint2 :: FileOrFolder -> IO ()
prettyPrint2 = trav file folder update 0
    where file   s (File a)    = putStrLn $ replicate s ' ' ++ "file " ++ show a
          folder s (Folder fs) = putStrLn $ replicate s ' ' ++ "Folder"
          update = (2+)





import Data.Char

{-
Skriv en funksjon
    gRep :: (t -> Bool) -> t -> [t] -> [t]
slik at gRep pr y xs erstatter ethvert element x i xs med y dersom det oppfyller predikatet pr.
-}

gRep :: (t -> Bool) -> t -> [t] -> [t]
gRep _  _ [] = []
gRep pr y (x:xs) | pr x = y : gRep pr y xs
                 | otherwise = x : gRep pr y xs

{-
Gjør det samme på nytt igjen, med map.
-}

gRep2 :: (t -> Bool) -> t -> [t] -> [t]
gRep2 pr y xs = map gRepHelper xs
    where 
        gRepHelper x | pr x = y
                     | otherwise = x

gRep3 :: (t -> Bool) -> t -> [t] -> [t]
gRep3 pr y xs = map (\x -> if pr x then y else x) xs


next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do 
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard board = mapM_ (\(row, n) -> putRow row n) $ zip [1..] board

putBoard2 :: Board -> IO ()
putBoard2 board = mapM_ (uncurry putRow) $ zip [1..] board

newline :: IO ()
newline = putChar '\n'

isNat :: String -> Bool
isNat "" = False
isNat s  = all isDigit s

play :: [Board] -> Int -> IO ()
play [] _ = error "bruh"
play (board:boards) player = do 
    newline
    putBoard board
    if finished board then do 
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do 
        newline
        putStr "Player "
        putStrLn (show player)
        inn <- getLine
        case words inn of
            (row:num:_) | isNat row && isNat num -> 
                if valid board (read row) (read num) 
                    then play (move board (read row) (read num) : board : boards) (next player)
                else putStrLn "Illegal move" >> play (board:boards) player
            ("0":_) | not (null boards) -> play boards (next player)
            ("save":_) -> save (board:boards) >> play (board:boards) player
            ("load":_) -> load >>= \boards -> play boards player
            _ -> do 
                newline
                putStrLn "ERROR: Invalid move"
                play (board:boards) player

save :: [Board] -> IO ()
save boards = writeFile "Nim.txt" $ unlines $ map (unwords . map show) boards

load :: IO [Board]
load = do
    fil <- readFile "Nim.txt"
    return (map (map read . words) (lines fil))

load2 :: IO [Board]
load2 = readFile "Nim.txt" >>= return . (map (map read . words)) . lines

nim :: IO ()
nim = play [initial] 1




import Data.Char (isDigit)
import Data.List (foldl')

type Tower = [Int]
type Hanoi = (Tower, Tower, Tower)
type Move  = (Int, Int)

hanoi :: Int -> Hanoi
hanoi n = ([1..n], [], [])

finished :: Hanoi -> Bool
finished ([], [], _) = True
finished _           = False

getNth :: Hanoi -> Int -> Tower
getNth (x, _, _) 1 = x
getNth (_, y, _) 2 = y
getNth (_, _, z) 3 = z
getNth _         _ = error "Feil indeks"

getAll :: Hanoi -> [Tower]
getAll h = map (getNth h) [1..3]

modifyNth :: Hanoi -> Int -> (Tower -> Tower) -> Hanoi
modifyNth (x, y, z) 1 f = (f x, y, z)
modifyNth (x, y, z) 2 f = (x, f y, z)
modifyNth (x, y, z) 3 f = (x, y, f z)
modifyNth _         _ _ = error "Feil indeks"

modifyAll :: Hanoi -> (Tower -> Tower) -> Hanoi
modifyAll h f = foldl' (\h n -> modifyNth h n f) h [1..3]

-- Denne burde returnert Maybe Int, men det er ikke pensum lenger :(
findTower :: Hanoi -> (Tower -> Bool) -> Int
findTower (x, y, z) p | p x = 1
                      | p y = 2
                      | p z = 3
                      | otherwise = error "Intet tårn oppfyller predikatet"

legal :: Hanoi -> Move -> Bool
legal h (f, t) = 
       all (`elem` [1..3]) [f, t] 
    && f /= t 
    && (not (null (getNth h f))) 
    && (null (getNth h t) || head (getNth h f) < head (getNth h t))

move :: Hanoi -> Move -> Hanoi
move h (f, t) = modifyNth (modifyNth h f tail) t (ring :)
    where ring = head (getNth h f)

undo :: Hanoi -> [Move] -> Hanoi
undo = foldl' (\h (f, t) -> move h (t, f))

height :: Hanoi -> Int
height = maximum . concat . getAll

printGamestate :: Hanoi -> String -> [Move] -> IO ()
printGamestate h message moves = do
    goto 0 0
    clearAll
    printHanoi paddedHanoi (height h * 2 + 1)
    putStrLn ("Moves made: " ++ show (length moves))
    putStrLn message
        where paddedHanoi = modifyAll h (\t -> replicate (height h + 1 - length t) 0 ++ t)

printHanoi :: Hanoi -> Int -> IO ()
printHanoi (x:xs, y:ys, z:zs) n = putStrLn (line x n ++ line y n ++ line z n) >> printHanoi (xs, ys, zs) n
printHanoi _                  _ = return ()

line :: Int -> Int -> String
line n_stars total = spaces ++ hashes ++ spaces
    where spaces = replicate ((total - length hashes) `div` 2) ' '
          hashes | n_stars <= 0 = "|"
                 | otherwise    = unwords $ replicate n_stars "#"

isNat :: String -> Bool
isNat "" = False
isNat s  = all isDigit s && (read s :: Int) > 0

main :: IO ()
main = do
    putStrLn "To start a game of size n, type 'b n'"
    inn <- getLine
    case words inn of
        ("q":_)             -> return ()
        ("b":n:_) | isNat n -> play (hanoi (read n)) "" []
        _                   -> putStrLn "Could not parse input." >> main

play :: Hanoi -> String -> [Move] -> IO ()
play h message moves = do
    printGamestate h message moves
    inn <- getLine
    case words inn of
        ("q":_)             -> return ()
        ("b":n:_) | isNat n -> play (hanoi (read n)) "" []
        ("z":n:_) | isNat n -> play (undo h (take (read n) moves)) "" (drop (read n) moves)
        ("h":_)             -> case ai h of
                                []         -> play h "You're already done!" moves
                                ((f, t):_) -> play h ("Hmmm. Maybe do " ++ show f ++ " -> " ++ show t ++ "?") moves
        (f:t:_) | isNat f && isNat t -> 
            if finished h then play h "You're already done!" moves
            else let m = (read f, read t)
                 in  if legal h m then if finished (move h m) then play (move h m) winningMessage (m:moves)
                                       else play (move h m) "" (m:moves)
                     else play h "Illegal move." moves
        _ -> play h "Could not parse input." moves
    where winningMessage = "Congratulations! \nYou finished a tower of height " ++ show (height h) ++ " in only " ++ show (length moves + 1) ++ " moves!"

ai :: Hanoi -> [Move]
ai h = snd $ flytt h (height h) 3 []

flytt :: Hanoi -> Int -> Int -> [Move] -> (Hanoi, [Move])
flytt h 0 _      moves = (h, moves)
flytt h n target moves | target == source = flytt h (n-1) target moves
                       | otherwise = let (nh, nmoves) = flytt h (n-1) helper moves
                                     in  flytt (move nh (source, target)) (n-1) target (nmoves++[(source, target)])
    where source = findTower h (n `elem`)
          helper = 6 - source - target



goto :: Int -> Int -> IO () --Flytter cursoren til koordinatene x y, med hensyn på terminalen
goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

clearAll :: IO() --Fjerner alt på skjermen i terminalen
clearAll = putStr "\ESC[2J"
