import System.IO
import Data.Char
-- Gudsteinn Arnarson, Group 3
-- problem 2.4: 10 (s 0 0, b 2 2) 1 1 2 1 2 2 
-- the specified glider is not really a glider but i was not able to find one

-- Data og Typer --
type Pos = (Int,Int)
data Celle =  Celle Pos Bool deriving (Eq)
type Board = [Celle]

-----------------------b---------s----
data Regel = Regel (Int,Int) (Int,Int)
getB :: Regel -> (Int,Int)
getB (Regel b _) = b
getS :: Regel -> (Int,Int)
getS (Regel _ s) = s
------------------------------------

getCellePos :: Celle -> Pos
getCellePos (Celle p _) = p
isAlive :: Celle -> Bool
isAlive (Celle _ b) = b
isAliveInt :: Celle -> Int
isAliveInt (Celle _ b) = if b == True then 1 else 0

getCellerFraBrett :: Board -> [Pos] -> [Celle]
getCellerFraBrett b [] = [] 
getCellerFraBrett b (p:ps) = 
    if isPosPaaBrett p b
        then [getCelleFraBrett b p] ++ getCellerFraBrett b ps  
    else getCellerFraBrett b ps

getCelleFraBrett :: Board -> Pos -> Celle
getCelleFraBrett [] pos = error "getCelleFraBrett: error cellen finnes ikke i brettet"
getCelleFraBrett (b:bs) pos = if (getCellePos b) == pos then b else getCelleFraBrett bs pos

getLevendeCeller :: Board -> [Celle]
getLevendeCeller x = [ c | c <- x, isAlive c] 
 
getPosFraBrett :: Board -> [Pos]
getPosFraBrett [] = []
getPosFraBrett (b:bs) = [getCellePos b] ++ getPosFraBrett bs

setCelleToAlive :: Pos -> Celle
setCelleToAlive p = (Celle p True)

celleFinnesPaaBrett :: Celle -> Board -> Bool
celleFinnesPaaBrett c [] = False
celleFinnesPaaBrett c (b:bs) = 
    if (getCellePos c) == (getCellePos b) 
        then True
    else
        celleFinnesPaaBrett c bs

isPosPaaBrett :: Pos -> Board -> Bool
isPosPaaBrett p [] = False
isPosPaaBrett p (b:bs) = 
    if p == (getCellePos b) 
        then True
    else
        isPosPaaBrett p bs

isPosPaaPos :: Pos -> [Pos] -> Bool
isPosPaaPos p [] = False
isPosPaaPos p (b:bs) = 
    if p == b
        then True
    else isPosPaaPos p bs

-----------------------------------------------------------------------------------------------

main :: IO ()
main = do
    let reg = (Regel (3,3) (2,3)) -- default regler
    clear
    writeAt (0,0)  "\n---------- Velkommen til Game of Life ----------\n" 
    loop reg

-- denne kjøres før en matrise er opprettet
loop :: Regel -> IO ()
loop reg = do 
    putStr "\ESC7" -- save position
    putStr "skriv din kommando\n"
    clearLine
    input <- getLine
    if input /= "" 
        then do inputHandler input reg
    else do 
        skrivMelding "du må opprette en matrise foer du starter game og life"
        loop reg

inputHandler :: String -> Regel -> IO ()
inputHandler "quit" _ = putStr "\nAvslutter program... \n"
inputHandler cmd reg
    -- opprett ny matrise
    | (cmd !! 0) == 'c' = do
        let mat = (read (tail cmd) :: Int)
        printBrett mat 
        let brett = c mat
        loop2 mat brett reg
    | (cmd !! 0) == 'n' || (cmd !! 0) == 'e' || (cmd !! 0) == 'l' || (cmd !! 0) == 'w' = do
        skrivMelding "En matrise maa bli opprettet for at en skal kunne bruke denne kommandoen"
        loop reg
    -- oppdater regler
    |(cmd !! 0) == 'b' || (cmd !! 0) == 's' = do
         let ch = (cmd !! 0)
         let pos = tail cmd
         let nyRegel = ((posList (strToIntList pos)) !! 0)
         let nyeRegler = oppdaterRegler reg ch nyRegel  
         skrivMelding "Reglene er oppdatert"
         loop nyeRegler
    -- se regler
    | (cmd !! 0) == '?' = do
       skrivMelding ("Reglene for spillet er s: " ++ (show (getS reg)) ++ " og b: " ++ (show (getB reg)))
       loop reg
    | (cmd !! 0) == 'r' = do
        let name = tail cmd
        r name
    | otherwise = do
        skrivMelding "kjenner ikke igjen kommandoen\n" 
        loop reg

-- denne kjøres etter en matrise er opprettet -------------------------------
loop2 :: Int -> Board -> Regel -> IO ()
loop2 mat brett reg = do
    putStr "\ESC7" -- save position
    putStr "skriv din kommando\n"
    clearLine
    input <- getLine
    if input /= "" 
        then do inputHandler2 input mat brett reg
    else do 
        gameOfLife mat reg brett
        loop2 mat brett reg

inputHandler2 :: String -> Int -> Board -> Regel -> IO ()
inputHandler2 "quit" _ _ _ = putStr "\nAvslutter program... \n"
inputHandler2 cmd mat brett reg
    -- lag bytt brett
    | (cmd !! 0) == 'c' = do
        let x = (read (tail cmd) :: Int)
        printBrett x
        let nyttBrett = c x 
        loop2 x nyttBrett reg
    -- lag nye levende celler på brettet
    | (cmd !! 0) == 'n' = do
        let pos = tail cmd
        if innenfor (strToIntList pos) mat 
            then do
                let posisjoner = posList (strToIntList pos)
                let levendeCeller = n posisjoner
                let nyttBrett = oppdaterBrett levendeCeller brett
                skriv "O" posisjoner mat
                loop2 mat nyttBrett reg
        else do 
            skrivMelding "Posisjonene du oppga var ikke gyldige\n"
            loop2 mat brett reg
    -- fjern levende celler på brettet
    | (cmd !! 0) == 'e' = do
        let pos = tail cmd
        if innenfor (strToIntList pos) mat 
            then do
                let posisjoner = posList (strToIntList pos)
                let deadCeller = e posisjoner
                let nyttBrett = oppdaterBrett deadCeller brett
                skriv "." posisjoner mat
                loop2 mat nyttBrett reg
        else do
            skrivMelding "Posisjonene du oppga var ikke gyldige\n"
            loop2 mat brett reg
    -- redefinerer reglene
    | (cmd !! 0) == 'b' || (cmd !! 0) == 's' = do
        let ch = (cmd !! 0)
        let pos = tail cmd
        let nyRegel = ((posList (strToIntList pos)) !! 0)
        let nyeRegler = oppdaterRegler reg ch nyRegel  
        skrivMelding "Reglene er oppdatert"
        loop2 mat brett nyeRegler
    -- se regler
    | (cmd !! 0) == '?' = do
        skrivMelding ("Reglene for spillet er s: " ++ (show (getS reg)) ++ " og b: " ++ (show (getB reg)))
        loop2 mat brett reg
    -- se levende celler
    | (cmd !! 0) == 'w' = do
        skrivMelding ("koordinatene til alle levende celler: ")
        w brett
        loop2 mat brett reg
    -- game of life live
    | (cmd !! 0) == 'l' = do
        let gen = read (tail cmd) :: Int
        gameOfLifeLive mat reg brett gen 0
    | (cmd !! 0) == 'r' = do
        let name = tail cmd
        r name
    | otherwise = do
        skrivMelding "kjenner ikke igjen kommandoen\n" 
        loop2 mat brett reg
------------------------------------------------------------------------------------------------------
-- c n
-- liste med døde celler
c :: Int -> Board
c n = [(Celle (x,y) False) | x <- [1..n], y <- [1..n]]

-- n x1 y1 ... xk yk 
n :: [Pos] -> [Celle] 
n [] = []
n (x:xs) = (Celle x True) : n xs
    
-- e x1 y1 ... xk yk 
e :: [Pos] -> [Celle]
e [] = []
e (x:xs) = (Celle x False) : n xs

-- b m n --
b :: Int -> Int -> (Int,Int)
b m n = (m,n)
-- s m n
s :: Int -> Int -> (Int,Int)
s m n = (m,n)

-- w
w :: Board -> IO ()
w xs = mapM_ (\y -> putStr (show (getCellePos y) ++ ", ")) (getLevendeCeller xs)

-- r name
r :: String -> IO ()
r name = do
    filen <- openFile (concat (words name)) ReadMode
    innhold <- hGetContents filen
    handleFile innhold
    hClose filen

handleFile :: String -> IO ()
handleFile f = do
    let x = read (parseMatrise f) :: Int
    printBrett x
    let brett = c x
    let defreg = (Regel (0,0) (0,0))
    let regel = parseRegelS f defreg 
    let reg = parseRegelB f regel
    let kommando = "n " ++ parseInput f 
    inputHandler2 kommando x brett reg 

parseMatrise :: String -> String
parseMatrise (x:y:xs) = if isDigit x && isDigit y then [x] ++ [y] else [x]

parseRegelB :: String -> Regel -> Regel
parseRegelB (x:y:n:z:m:xs) r  
    | x == 'b' = 
        if isDigit n && isDigit m then
            oppdaterRegler r x (digitToInt  n, digitToInt  m)
        else error "noe gikk galt i parseRegel"  
    | otherwise = parseRegelB (y:n:z:m:xs) r
parseRegelB (x:xs) r = parseRegelB xs r

parseRegelS :: String -> Regel -> Regel
parseRegelS (x:y:n:z:m:xs) r  
    | x == 's' =
            if isDigit n && isDigit m then
                oppdaterRegler r x (digitToInt  n, digitToInt  m)
            else error "noe gikk galt i parseRegel"
    | otherwise = parseRegelS (y:n:z:m:xs) r
parseRegelS (x:xs) r = parseRegelS xs r

parseInput :: String -> String
parseInput [] = []
parseInput [x] = [x]
parseInput (x:xs) = 
    if x == ')' 
        then parsePunkt xs 
    else parseInput xs  

parsePunkt :: String -> String
parsePunkt [x] = [x]
parsePunkt (x:xs) = x : xs

------------------------------------------------------------------------------------------------
-- Regler
oppdaterRegler :: Regel -> Char -> (Int,Int) -> Regel
oppdaterRegler r c (m,n) = 
    if c == 'b'
        then (Regel (m,n) (getS r))
    else if c == 's'
        then (Regel (getB r) (m,n))
    else 
        error "kommandoen kan ikke brukes til å oppdatere Regel"
------------------------------------------------------------------------------------------------
-- gir tilbake antall levende naboer til en celle --
antLevendeNabo :: Celle -> Board -> Int
antLevendeNabo c b = 
    if celleFinnesPaaBrett c b
        then sjekkLevendeNaboer c b
    else 0

sjekkLevendeNaboer :: Celle -> Board -> Int    
sjekkLevendeNaboer c b = do 
    let (x,y) = getCellePos c
    let naboliste = [(x-1, y-1), (x,y-1),(x+1, y-1),(x-1, y),(x+1, y),(x-1, y+1),(x,y+1),(x+1, y+1)]
    let brettPos = getPosFraBrett b
    let hentedeCeller = getCellerFraBrett b naboliste
    sum $ map (\a -> isAliveInt a) hentedeCeller

---------------------------------------------------------------------------------------------

overlever :: Regel -> Board -> Board -> [Celle]
overlever reg [b] a = if (isAlive b) then [overlevCelle reg b a] else []
overlever reg (b:bs) a
    | isAlive b = -- lever
        overlevCelle reg b a : overlever reg bs a
    | otherwise = overlever reg bs a     

overlevCelle :: Regel -> Celle -> Board -> Celle
overlevCelle reg c b =
    if ((antLevendeNabo c b) >= (fst (getS reg)) && (antLevendeNabo c b) <= (snd (getS reg))) 
        then c -- cellen overlever
    else (Celle (getCellePos c) False) -- celle dør

-- født --
born :: Regel -> Board -> Board -> [Celle]
born reg [c] a  
    | (not (isAlive c)) =
        if ((antLevendeNabo c a) >= (fst (getB reg)) && (antLevendeNabo c a) <= (snd (getB reg))) -- blir født
            then [(Celle (getCellePos c) True)] -- legger til nyfødt
        else []  -- er forsatt død (ignorerer) 
    | otherwise = []

born reg (b:bs) a 
    | not (isAlive b) = --er død
        --bornCelle reg b a : born reg bs a
        if ((antLevendeNabo b a) >= (fst (getB reg)) && (antLevendeNabo b a) <= (snd (getB reg))) -- blir født
            then (Celle (getCellePos b) True) : born reg bs a -- legger til nyfødt
        else born reg bs a  -- er forsatt død (ignorerer)
    | otherwise = born reg bs a 


------------------------------------------------------------------------------------------------
-- GAME OF LIFE --
gameOfLifeLive :: Int -> Regel -> Board -> Int -> Int -> IO ()
gameOfLifeLive n reg b x acc = do
    let survBrett = overlever reg b b
    let bornBrett = born reg b b
    let nesteCeller = survBrett ++ bornBrett
    let nextGen = oppdaterBrett nesteCeller b
    skrivUpdate nextGen n
    if n < 10 then do vent 100000 else do vent 1
    if nextGen == b || x == acc then do
        clearLine
        writeAt (n+3,0) ("spillet er ferdig etter " ++ (show acc) ++ " generasjoner\n")
        loop2 n nextGen reg
    else do
        gameOfLifeLive n reg nextGen x (acc+1)

gameOfLife :: Int -> Regel -> Board -> IO ()
gameOfLife n reg b = do
    let survBrett = overlever reg b b
    let bornBrett = born reg b b
    let nesteCeller = survBrett ++ bornBrett
    let nextGen = oppdaterBrett nesteCeller b
    skrivUpdate nextGen n
    if n < 10 then do vent 100000 else do vent 1
    if nextGen == b then do
        clearLine
        writeAt (n+3,0) ("spillet har komme til en stabil generasjon\n")
        loop2 n nextGen reg
    else do
        loop2 n nextGen reg


skrivUpdate :: Board -> Int -> IO ()
skrivUpdate b n = mapM_ (\c -> skriv (if (isAlive c) then "O" else ".") [(getCellePos c)] n) b

-------------------------------------------------------------------------------------------------
-- lager et nytt brett med celler som er "oppdaterte" til levende eller døde
oppdaterBrett :: [Celle] -> Board -> Board
oppdaterBrett [] bs = bs
oppdaterBrett (c:cs) b = do
    let oppdatert = oppdaterCellePaaBrett c b
    oppdaterBrett cs oppdatert  
    
oppdaterCellePaaBrett :: Celle -> Board -> Board
oppdaterCellePaaBrett c [] = [c] 
oppdaterCellePaaBrett c (b:bs) = 
    if (getCellePos c) == (getCellePos b) 
        then c : bs
    else b : oppdaterCellePaaBrett c bs

-----------------------------------------------------------------------------------------------------

-- UTSKRIFT -----------------------------------------------------------------------------------------
printBrett :: Int -> IO ()
printBrett nR = 
    if nR < 1 || nR > 99 
        then do 
            skrivMelding "matrise storrelsen kan ikke være utenfor: 1 <= n <= 99"
    else 
        do 
            clear
            forsteLinje nR
            mapM_ (\i -> nLinje i nR) [1..nR]
            clearLine
            putStr "\ESC7" -- save position
            

forsteLinje :: Int -> IO ()
forsteLinje n = 
    if n <= 9 
        then writeAt (0, ve + 1) (concat [(show i) ++ " " | i <- [1..n]] ++ "\n") 
    else writeAt (0, ve + 2) (concat ([(show i) ++ "  " | i <- [1..9]] ++ [(show i) ++ " " | i <- [10..n]]) ++ "\n") 

nLinje :: Int -> Int -> IO ()
nLinje i n = do
    writeAt (1+i, if i > 9 then (ve - 2) else (ve - 1)) (show i)
    if n <= 9 then mapM_ (\i -> putStr " .") [1..n] else mapM_ (\i -> putStr "  .") [1..n]
    putStrLn ""

skriv :: String -> [Pos] -> Int -> IO ()
skriv _ [] _ = return ()
skriv str xs mat = 
    if mat <= 9 
        then do 
            mapM_ (\(x,y) -> writeAt (x + 1, y * 2 + 2) str) xs
            putStr "\ESC8" -- restore position
    else do 
        mapM_ (\(x,y) -> writeAt (x + 1, y * 3 + 2) str) xs
        putStr "\ESC8" -- restore position

skrivMelding :: String -> IO ()
skrivMelding str = do 
    clearLine
    putStr (str ++ "\n")
    putStr "\ESC8" -- restore position

writeAt :: (Show a1, Show a2) => (a1, a2) -> String -> IO ()
writeAt (x,y) xs = do 
    goto x y
    putStr xs

showCeller :: [Celle] -> IO ()
showCeller xs = do putStr (concat (map (\y -> (show (isAliveInt y) ++ (show (getCellePos y) ++ " "))) xs))

-----------------------------------------------------------------------------------------------------
-- Hjelpemetoder --
innenfor :: [Int] -> Int -> Bool
innenfor xs n = 
    if (length xs) `mod` 2 == 0 && all (<= n) xs 
        then True
    else False  

posList :: [Int] -> [Pos]
posList [] = []
posList [x,y] = [(x,y)] 
posList (x:y:xs) = [(x,y)] ++ posList xs

strToIntList :: String -> [Int]
strToIntList str = map (\x -> read x :: Int) (words str)

goto :: (Show a1, Show a2) => a1 -> a2 -> IO ()
goto x y = putStr ("\ESC["++ show x ++ ";"++ show y ++ "H")

ve :: Integer
ve = 3

clearLine :: IO ()
clearLine = putStr "\ESC[K"

clear :: IO ()
clear = putStr "\ESC[2J"

vent :: Int -> IO ()
vent n = sequence_ [return () | _ <- [1..n]]