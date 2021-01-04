-- 1 --
-- a) rekursjon
alRec :: [Bool] -> Bool
alRec [] = True
alRec [x] = if x == True then True else False
alRec (x:xs) = 
    if x == True
        then alRec xs
    else False

-- b) innebygget funksjon
alInnebygget :: [Bool] -> Bool
alInnebygget x = and x

-- c) foldl
alFoldl :: [Bool] -> Bool
alFoldl = foldl (&&) True 

-- d) foldr
alFoldr :: [Bool] -> Bool
alFoldr = foldr (&&) True

-- 2 --
ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala f b [] = if foldr f b [] then True else False 
ala f b liste = foldr f b liste

-- 3 --
trekant :: Int -> IO()
trekant 0 = putStr ""
trekant x = putStrLn (reverse (juletre x))

juletre :: Int -> String
juletre 1 = "*"
juletre x = antStjerner x ++ juletre (x - 1) 

antStjerner :: Int -> String
antStjerner 1 = "*\n" 
antStjerner x = "*" ++ antStjerner (x - 1) 

