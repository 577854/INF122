
-- A --
-- listekomprehensjon
fjern :: String -> Char -> String
fjern xs a = [x | x <- xs, a /= x]

-- rekusjon
fjernRec :: String -> Char -> String
fjernRec [] a = []
fjernRec (x:xs) a = if x == a then fjernRec xs a else x : fjernRec xs a 

-- labben
--fjern3 :: String -> Char -> String
--fjern3 (x:xs) a = filter (/= a) xs --feeeil

-- B --
-- listekomprehensjon
tegnpos :: Char -> String -> [Int]
tegnpos a xs = [y | y <- [0..((length xs) - 1)], a == (xs !! y)]

-- rekusjon -- fikk ikke til denne
--tegnpos' :: Char -> String -> [Int]
--tegnpos' a [] = []
--tegnpos' a (x:xs) = if a == x then {-hvordan i all verden finner man indeksen i en liste rekursivt??????-} : tegnpos a xs else tegnpos a xs
tegnpos1 :: Char -> String -> [Int]
tegnpos1 c xs = tegnpos1' c (zip xs [0..])
    where
        tegnpos1' :: Char -> [(Char, Int)] -> [Int]
        tegnpos1' _ [] = []
        tegnpos1' c ((x,n):xs) = if x == c then n : tegnpos1' c xs else tegnpos1' c xs

-- C --
intToList :: Int -> [Int]
intToList 0 = []
intToList a = intToList (a `div` 10) ++ [a `mod` 10]

--labben
intToList2 :: Int -> [Int]
intToList2 n = 
    let 
        digit = n `mod` 10
        n' = n `div` 10
    in if n' < 10 then [digit] else (intToList2 n' ++ [digit]) -- mangler første elementet..

-- D.a --
settSammen :: [String] -> String
settSammen [] = []
settSammen (x:xs) = x ++ " " ++ settSammen xs

-- D.b --
delStrengen :: String -> [String]
delStrengen a 
    | length a == 0 = [] 
    | a !! 0 == ' ' = delStrengen (tail a)
    | otherwise = [fst (break(' '==)a)] ++ delStrengen (snd (break(' '==)a))

-- D.c -- fikk ikke til denne
gdelStrengen :: String -> String -> [String]
gdelStrengen a cond
    | length a == 0 = [] 
    | length cond == 0 = [a]
    | (cond !! 0) `notElem` a = gdelStrengen a (tail cond)
    | otherwise = [fst (break((cond !! 0)==)a)] ++ gdelStrengen (snd (break((cond !! 0)==)a)) (tail cond)

--labben
--gdelStrengen2 :: String -> String -> [String]
--gdelStrengen2 xs ys = (gdelStrengen' (dropwhile (`elem` ys) xs) ys)
--    where
--        gdelStrengen' :: String -> String -> [String]
--        gdelStrengen' [] _ = []
--        gdelStrengen' xs ys =
--            let 
--                (prefix, rest) = break (`elem` ys)xs
--                rest' = dropwhile (`elem` ys) rest
--            in prefix : gdelStrengen' rest' ys

