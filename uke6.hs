--A
remg :: [a] -> (a -> Bool) -> [a]
remg [] p = [];
remg (x:xs) p | p x       = xs
              | otherwise = x : remg xs p;

--7.9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f p [] = [];
altMap f p [x] = [f x];
altMap f p (x:y:xs) = f x : p y : altMap f p xs;