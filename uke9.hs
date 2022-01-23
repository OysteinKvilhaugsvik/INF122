import Data.Char ( isDigit )

--A

--Med map
gRep::(t->Bool)->t->[t]->[t]
gRep pr y [] = [];
gRep pr y (x:xs) = map (\a -> if pr a then y else a) (x:xs)

--Uten map
gRep'::(t->Bool)->t->[t]->[t]
gRep' pr y [] = [];
gRep' pr y (x:xs)
            | pr x = y : gRep' pr y xs
            | otherwise = x : gRep' pr y xs

--D


next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]
type Moves = [(Int, Int)]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num


move :: [Int] -> (Int, Int) -> Int -> Int -> [Int]
move board moves row num = do
    [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do
    putRow 1 a
    putRow 2 b
    putRow 3 c
    putRow 4 d
    putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do
    putStr prompt
    s <- getLine
    newline
    if not (null s) && all isDigit s then return (read s)
    else do
        putStrLn "ERROR: Invalid row"
        getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
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
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove : "
        if valid board row num then play (move board (row,num) row num) (next player)
        else do
            newline
            putStrLn "ERROR: Invalid move"
            play board player

nim :: IO ()
nim = play initial 1