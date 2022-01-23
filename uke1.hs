--g
addobb :: [Int] -> [Int]
addobb [] = [];
addobb xs = xs ++ map (*2) xs;


--h
pali :: Eq a => [a] -> Bool
pali xs = xs == reverse xs