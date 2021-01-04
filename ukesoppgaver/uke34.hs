-- B --
-- CHAPTER 1
-- exersise 1.4:
qSortRev [] = []
qSortRev (x:xs) = qSortRev larger ++ [x] ++ qSortRev smaller
                    where 
                        larger = [a | a <- xs, a >= x]
                        smaller = [b | b <- xs, b < x]

--exersice 1.5
{- 
vi ser på eksempelet qsort [2,2,3,1,1]:

1:    utfører qsort 
qsort [1,1] ++ [2] ++ qsort [3] 

2:    utfører qsort
(qsort [] ++ [1] ++ qsort []) ++ [2] ++ [3]

3:    utfører qsort
[] ++ [1] ++ [] ++ [2] ++ [3]

4:    utfører operasjonen ++
[1,2,3]

Her kan vi se at resultatet eksluderer verier som sammelignes med pivoten [x] er like.
For eksempel i steg 2 ser vi at "smaller" i algoritmen til qsort, velger bort 1 fordi 1 < 1 ikke stemmer
-}

-- CHAPTER 2 
-- exersise 2.4:
myLast xs = xs !! (length xs - 1)

-- exersice 2.5:
myInit xs = reverse( drop 1 (reverse xs))

myInit1 xs = take (length xs - 1) xs

-- C --
-- 1.
plu :: [Int] -> Int -> [Int]
plu [] n = []
plu [k] n = [k+n]
plu (k:ks) n = plu [k] n ++ plu ks n 

-- 2.
pali :: (Eq a) => [a] -> Bool
pali xs = xs == (reverse xs)