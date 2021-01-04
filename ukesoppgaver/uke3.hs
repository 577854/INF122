import Data.List ( (\\) ) -- for oppgave D

-- A --
-- 4.5 (kan også bruke && istedenfor and som funksjonsnavn)
and :: Bool -> Bool -> Bool
and x y = if x == True then
        if y == True then True else False
    else False

-- 4.7
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

multLamb :: Int -> Int -> Int -> Int
multLamb = \x -> \y -> \z -> x*y*z

-- 5.6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects x = [y | y <- [1..x], (sum (init (factors y))) == y]

-- 5.7
myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x <- xs]

oppgave57 :: [(Integer, Integer)]
oppgave57 = myConcat [[(x,y) | y <- [3,4] ] | x <- [1,2]] 
oppgaven :: [(Integer, Integer)]
oppgaven = [(x,y) | x <- [1,2], y <- [3,4]] {- <---kan brukes for å sjekke at begge gir samme svar -}

-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- C --
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) y = if x == y then xs else x : rem1 xs y 
-- en annen måte å gjøre det på, se nedenfor
--rem1 (x:xs) y 
--            | x == y = xs 
--            | otherwise = x : rem1 xs y

-- D -- 
diff :: Eq a => [a] -> [a] -> [a]
diff as [] = as
diff as (b:bs) = diff (rem1 as b) bs  
-- gjorde ikke den som er ovenfor. dette er fra labben
--diff as bs = as \\ bs

-- mer lab nedenfor
-- E --
luhnDouble :: Int -> Int
luhnDouble n 
    = let m = n * 2
    in if m > 9 then m - 9 else m

luhnDouble' :: Int -> Int
luhnDouble' n = if m > 9 then m - 9 else m
    where m = n * 2

{-luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d e 
    = let 
        res = a' + b + c' + d 
        a' = luhnDouble a
        c' = luhnDouble c 
      in 
        res `mod` 10 == 0 -}