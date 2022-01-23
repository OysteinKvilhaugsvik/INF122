-- Øystein Kvilhaugsvik
module Oblig1 where

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
isPrefix [] ys = True;
isPrefix xs [] = False;
isPrefix  xs ys = xs == ysHead
            where ysHead = take (length xs) ys;


-- Oppgave 2 ----------------------------------------------------
locate :: String -> String -> [(Int,Int)]
locate [] _ = [];
locate _ [] = [];
locate xs (y:ys) = if isPrefix xs (y:ys) then (0, length xs): locate' xs ys 1
               else locate' xs ys 1

locate' :: String -> String -> Int -> [(Int, Int)]
locate' _ [] _ = [];
locate' xs (y:ys) x = if isPrefix xs (y:ys) then (x,x + length xs): locate' xs ys (x + 1)
               else locate' xs ys (x + 1);


-- Oppgave 3 ----------------------------------------------------
translate :: String -> String
translate "" = "";
translate xs = if xs `elem` head (map snd dictionary) then
                head (map fst dictionary)
                else translate' xs 1;

translate' :: String -> Int -> String
translate' xs 29 = if xs `elem` map snd dictionary !! 29 then
                       concat $ replicate (length xs) "*";
                else "";

translate' xs x = if xs `elem` map snd dictionary !! x then
                   map fst dictionary !! x
                   else translate' xs (x+1);


-- Oppgave 4 ----------------------------------------------------
replace :: [(Int,Int)] -> String -> String
replace [] ys = ys;
replace _ [] = [];
replace (x:xs) ys = take (fst x) ys ++ translate (take (snd x)(drop (fst x) ys)) ++
                    replace' xs (drop (snd x) ys) (snd x);

replace' :: [(Int,Int)] -> String -> Int -> String
replace' [] ys _ = ys;
replace' _ [] _ = [];
replace' (x:xs) ys tupp2 = take (fst x - tupp2) ys ++
                           translate (take (snd x - fst x) (drop (fst x-tupp2) ys)) ++
                           replace' xs (drop (snd x - tupp2) ys) (snd x);


-- Oppgave 5 ----------------------------------------------------
toNewspeak :: String -> String
toNewspeak xs = replace (qsort (toNewspeak' (allWords dictionary) xs)) xs;

allWords :: [(String, [String])] -> [String]
allWords [] = []
allWords ((_,b):dict) = b ++ allWords dict;

toNewspeak' :: [String] -> String -> [(Int, Int)]
toNewspeak' [] _ = [];
toNewspeak' (x:xs) ys = locate x ys ++ toNewspeak' xs ys;


-- Oppgave 6 ----------------------------------------------------
analytics :: String -> String -> Int
analytics [] _ = 0;
analytics xs [] = 100;
analytics xs ys = round(100 * (letters / lengthStr))
                        where letters = fromIntegral (analytics' (qsort' xs) (qsort' ys) 0) :: Float
                              lengthStr = fromIntegral(length xs) :: Float

analytics' :: String -> String -> Int -> Int
analytics' [] _ count = count;
analytics' xs [] count = length xs + count;
analytics' (x:xs) (y:ys) count
  | x == y = analytics' xs ys count
  | x > y = analytics' (x:xs) ys count
  | otherwise = analytics' xs (y:ys) (count+1);


-- Oppgave 7 ----------------------------------------------------
--main :: String -> (String, Int)
--main xs = toNewspeak xs analytics xs (toNewspeak xs);


-- Qsort ----------------------------------------------------
qsort :: [(Int, Int)] -> [(Int, Int)]
qsort [] = [];
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b > x]
qsort' :: String -> String
qsort' [] = [];
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]
