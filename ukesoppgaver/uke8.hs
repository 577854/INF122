-- alRec, alInnebygget, alFoldl, alFoldr :: [Bool] -> Bool
-- ^ annen måte å skrive små funksjoner med samme input/output
-- 1 --
-- a) rekursjon
alRec :: [Bool] -> Bool
alRec [] = True
alRec [x] = if x == True then True else False
alRec (x:xs) = 
    if x == True
        then alRec xs
    else False
-- alRec (x:xs) = x && alRec xs -- <- kan skrives slik også

-- b) innebygget funksjon
alInnebygget :: [Bool] -> Bool
alInnebygget x = and x
-- alInnebygget = and -- <- kan skrives slik også

-- c) foldl
alFoldl :: [Bool] -> Bool
alFoldl = foldl (&&) True 
-- alfFoldl xs = foldl (&&) True xs -- <- kan skrives slik også (samme for foldr)

-- d) foldr
alFoldr :: [Bool] -> Bool
alFoldr = foldr (&&) True

-- 2 --
ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala f b [] = if foldr f b [] then True else False -- trenger ikke denn linjen
ala f b liste = foldr f b liste
-- ala = foldl -- <- kan skrives slik også

-- 3 --
trekant :: Int -> IO()
trekant x = putStrLn (reverse (trekant' x))

trekant' :: Int -> String
trekant' 1 = "*"
trekant' x = antStjerner x ++ trekant' (x - 1) 

antStjerner :: Int -> String
antStjerner 1 = "*\n" 
antStjerner x = "* " ++ antStjerner (x - 1) 

-- alternativ fra lab
trekantLab :: Int -> IO()
trekantLab height 
    | height >= 0 = trekantLab' height 1
    | otherwise = error "the heirght must be at least 0!"

trekantLab' :: Int -> Int -> IO()
trekantLab' 0 _ = return () -- avslutt funksjon (pakk tom tuppel inn i IO)
trekantLab' height width = do
    let line = unwords (replicate width "*") 
    putStrLn line 
    trekantLab' (height - 1) (width + 1)

-- alternativ 2 fra lab (rekursivt)
trekantlab :: Int -> IO()
trekantlab 0 = return ()
trekantlab height = do
    trekantlab (height - 1)
    putStrLn $ unwords $ replicate height "*"

-- 4 --
-- kaller denne metoden for juletre og ikke trekant
juletre :: Int -> IO()
juletre x = putStrLn (reverse (foldr (++) "" (juletre' x x)))

juletre' :: Int -> Int -> [String]
juletre' 1 x = ["*"++ (mellomrom (x - 1))]
juletre' x y = (antStjerner' x ++ mellomrom (y - x) ++ "\n") : (juletre' (x - 1) y)

antStjerner' :: Int -> String
antStjerner' 1 = "*" 
antStjerner' x = "* " ++  antStjerner' (x - 1)

mellomrom :: Int -> String
mellomrom 0 = ""
mellomrom x = " " ++ mellomrom (x - 1)

-- fra lab
xmasTree :: Int -> IO()
xmasTree height
    | height >= 0 = xmasTree' height  1
    | otherwise = error "your christmas tree cannot be negatove!"

xmasTree' :: Int -> Int -> IO()
xmasTree' 0 _ = return ()
xmasTree' height width = do
    putStrLn $ padLine height width
    xmasTree' (height - 1) (width + 1)

padLine :: Int -> Int -> String
padLine height width = 
    let line = unwords $ replicate width "*"
    in replicate (height - 1) ' ' ++ line