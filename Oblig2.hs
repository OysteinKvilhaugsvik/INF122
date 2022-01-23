import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List (findIndex, elemIndex)
import Text.ParserCombinators.Parsec (newline)


towers :: Tower -> Tower -> Tower -> IO ()
towers tower t2 t3 = mapM_ (\m -> putStrLn $ towerline (tower !! m) m ++ towerline (t2 !! m) m ++ towerline (t3 !! m) m) [0..height]
    where height       = maximum [last tower, last t2, last t3]
          towerline n m = line n (height*2+1)

line :: Int -> Int -> String
line disc_size total | disc_size <= 0 = replicate (total `div` 2) ' ' ++ "|" ++ replicate (total `div` 2) ' '
                   | otherwise    = spaces ++ stars ++ spaces
    where stars = unwords $ replicate disc_size "#"
          spaces = replicate ((total - length stars) `div` 2) ' '

drawTowers :: Board -> IO ()
drawTowers board = towers (head board) (board !! 1) (board !! 2);

type Tower = [Int]
type Board = [Tower]

putBoard :: Int -> Board
putBoard size = [[0..size], replicate (size+1) 0, replicate (size+1) 0]

moveDisc :: Board -> Int -> Int -> (Board, Int)
moveDisc board tower new_disc = if new_disc == 0 then
                                let newTower = replaceElement (board !! tower) disc_index new_disc
                                    disc_index = fromMaybe new_disc $ findIndex (>0) (board !! tower) in
                                      (replaceTower board tower newTower, (board !! tower) !! disc_index)

                                else let newTower = replaceElement (reverse (board !! tower)) disc_index new_disc
                                         disc_index = fromMaybe new_disc (elemIndex 0 (reverse (board !! tower)))
                                         replaced_index = fromMaybe new_disc $ findIndex (>0) (board !! tower) in
                                      (replaceTower board tower (reverse newTower), (board !! tower) !! replaced_index)

replaceTower :: Board -> Int -> Tower -> Board
replaceTower board i e = before ++ [e] ++ after
                    where (before, _:after) = splitAt i board

replaceElement :: Tower -> Int -> Int -> Tower
replaceElement tower i e = before ++ [e] ++ after
                    where (before, _:after) = splitAt i tower

displayBoard :: Show a => Board -> a -> String -> IO ()
displayBoard board moves message = do
          putStr "\ESC[2J"
          drawTowers board
          let movesStr = show moves
          putStr ("Number of Moves: " ++ movesStr ++ message ++ "\n")

undoMoves :: [Board] -> Int -> [Board]
undoMoves boardList n_undo =
                            let remainder = reverse (drop n_undo (reverse (tail boardList))) in
                            head boardList : remainder

hanoi :: Board -> [Board] -> Int -> IO ()
hanoi board boardList moves = do
        c <- getLine
        let input = words c
        if head input == "q" then return()

        else if head input == "b" then

          if null (last input) || (read (last input) :: Int) == 0 then
            main

          else do
          let size = read (last input) :: Int
              board = putBoard size
              boardList = [board]
          displayBoard board moves ""
          hanoi board boardList moves

        else if head input == "z" then

          if null board || null (last input) then
            main

          else do
          let n_undo = read (last input) :: Int
              newBoardList = undoMoves boardList n_undo
              newBoard = last newBoardList
              newMoves = if moves > n_undo then moves - n_undo
                         else 0
          displayBoard newBoard newMoves ""
          hanoi newBoard newBoardList newMoves

        else if isDigit (head (unwords input)) then

          if null board || null input || null (last input) then
            main

          else do
          let t1 = (read (head input) :: Int) - 1
              t2 = (read (last input) :: Int) - 1

          if notElem t1 [0,1,2] || notElem t2 [0,1,2] || null board then do

            displayBoard board moves "       Choose between 1-3"
            hanoi board boardList moves

          else do
          let removeDisc = moveDisc board t1 0
              addDisc = moveDisc (fst removeDisc) t2 (snd removeDisc)

          if (snd removeDisc > snd addDisc && (snd addDisc /= 0)) ||
             snd removeDisc == 0 || t1 == t2 then do
            displayBoard board moves "       Not valid move"
            hanoi board boardList moves

          else do
            displayBoard (fst addDisc) (succ moves) ""

            if reverse (fst addDisc) == head boardList then do
              putStrLn "########### Congrats! ###########"
              putStr "Disc level: "
              print (length (head board) - 1)
              putStr "Moves Used: "
              print (succ moves)
              main

            else do
            hanoi (fst addDisc) (boardList ++ [fst addDisc]) (succ moves)

        else if null board || null boardList then do
                putStr "\ESC[2J"
                main

        else do
                displayBoard board moves "       Invalid Command"
                hanoi board boardList moves

main :: IO ()
main = do
        putStrLn "Start a new game with: b <nbOfRings>, or quit with q"
        hanoi [] [] 0;

