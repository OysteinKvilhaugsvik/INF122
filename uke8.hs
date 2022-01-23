import Data.Char

--B

trekant :: Int -> IO ()
trekant num = do
        sequence_ [putStrLn (concat (replicate (num-i) " " ++ replicate i "* " ++ replicate
         (num-i) " ")) | i <- [1..num]]
         
--C

trekanter :: Int -> Int -> Int -> IO ()
trekanter num1 num2 num3 = do 
                           trekant num1
                           trekant num2
                           trekant num3

--D
data FileOrFolder = File | Folder [FileOrFolder]

prettyPrint :: FileOrFolder -> IO ()
prettyPrint structure = putStr (pp structure)

pp :: FileOrFolder -> String
pp structure = draw structure ""

indent = "  ";

draw :: FileOrFolder -> String -> String
draw (Folder (x:xs)) ind = ind ++ "-Folder\n" ++ draw x indent ++ "\n" ++ ind ++ draw (head xs) indent
draw File ind = ind ++ "-File" 
draw _ _ = [];


                        
