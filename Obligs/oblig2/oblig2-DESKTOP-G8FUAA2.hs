import System.IO
-- Gudsteinn Arnarson, Group 3
-- problem 2.4:

-- Data og Typer --
type Pos = (Int,Int)
data Celle =  Celle Pos Bool 
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
    let reg = (Regel (3,3) (2,3)) -- placeholder TODO hent regel fra en annen plass??
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
    | (cmd !! 0) == 'c' = do
        let mat = (read (tail cmd) :: Int)
        printBrett mat 
        let brett = c mat
        loop2 mat brett reg
    | (cmd !! 0) == 'n' || (cmd !! 0) == 'e' = do
        skrivMelding "En matrise maa bli opprettet for at en skal kunne bruke denne kommandoen"
        loop reg
    |(cmd !! 0) == 'b' || (cmd !! 0) == 's' = do
         let ch = (cmd !! 0)
         let pos = tail cmd
         let nyRegel = ((posList (strToIntList pos)) !! 0)
         let nyeRegler = oppdaterRegler reg ch nyRegel  
         skrivMelding "Reglene er oppdatert"
         loop nyeRegler
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
                --test -------------
                --putStr "hei\n"
                --putStr "hei\n"
                --showCeller levendeCeller
                --putStr "\n"
                --putStr "nytt brett:\n"
                --showCeller nyttBrett
                --putStr "\n"
                --putStr "gammelt brett:\n"
                --showCeller brett
                ---------------------
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
    | (cmd !! 0) == '?' = do
        skrivMelding ("Reglene for spillet er s: " ++ (show (getS reg)) ++ " og b: " ++ (show (getB reg)))
        loop2 mat brett reg
    | (cmd !! 0) == 'w' = do
        skrivMelding ("koordinatene til alle levende celler: ")
        w brett
        loop2 mat brett reg
    | otherwise = do
        skrivMelding "kjenner ikke igjen kommandoen\n" 
        loop2 mat brett reg
------------------------------------------------------------------------------------------------------
-- c n
-- liste med posisjoner eller liste med døde celler??? hmmmm????
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
-- TODO få det til å vises på en riktig plass..
w :: Board -> IO ()
w xs = mapM_ (\y -> putStr (show (getCellePos y) ++ ", ")) (getLevendeCeller xs)
-- r name
--r :: String -> IO ()
--r name = do
--    filen <- openFile name ReadMode
--    innhold <- hGetContents filen
--    clear
--    handleFile innhold
--    hClose filen
--
--handleFile f = do
--    let x = read (fil !! 0) :: Int 
--    printBrett x
--    let brett = c x -- Board
--    let n = read (fil !! 5) :: Int
--    let m = read (fil !! 7) :: Int
--    let a = read (fil !! 12) :: Int
--    let b = read (fil !! 14) :: Int
--    if (fil !! 2) == '(' && (fil !! 3) == 'b'
--        then do 
--            let reg = (Regel (n,m) (a,b)) -- Regel
--    else if (fil !! 3) == 's' 
--        then do
--            let reg = (Regel (a,b) (n,m)) -- Regel
--    else error "noe gikk galt i handleFile" -- TODO skriv om til skrivMelding og gå tilbake til loop hvis alt funker
--    if (fil !! 15) == ')' 
--        then do
--            let kommando = "n " ++ parsePunkter fil 
--    inputHandler2 kommando x brett reg 

parsePunkter :: String -> String
parsePunkter [] = []
parsePunkter [x] = [x]
parsePunkter (x:xs) = if x == ')' then parseP xs else parsePunkter xs 

parseP :: String -> String
parseP [x] = [x]
parseP (x:xs) = x : xs
    
--5 (s 2 3, b 3 3) 3 3 1 3 2 1 3 2 2 3

-- l x
--TODO
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
    ---test
    --putStr "\n"
    --putStr "brett\n"
    --showCeller b
    --putStr "\n"
    --------
    let survBrett = overlever reg b b
    let bornBrett = born reg b b
    let nesteCeller = survBrett ++ bornBrett
    let nextGen = oppdaterBrett nesteCeller b
    --test
    --putStr "nesteBrett\n"
    --showCeller nextGen
    ------
    skrivUpdate nextGen n
    --vent 10000000
    if nextGen == b || x == acc then do
        skrivMelding "spillet er ferdig etter " ++ (show acc) ++ " generasjoner"
        loop2 n nextGen reg
    else do
        gameOfLifeLive n reg nextGen x (acc+1)


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
    if nR <= 1 || nR >= 99 
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


--TEST -- TEST -- TEST -- TEST -- TEST -- TEST -- TEST -- TEST -- TEST -- TEST -- TEST -- TEST -
punkter :: [Pos]
punkter = [(1,1),(1,2),(2,1),(2,2)]

cell :: [Celle]
cell = n punkter

--lev :: Celle
--lev = setCelleToAlive (getPosFraBrett cell)

prov :: [Celle] -> String
prov c = concat ((map (\x -> (show (isAliveInt x) ++ (show (getCellePos x) ++ " ")))) c)

posis :: String -> [Pos]
posis pos = posList (strToIntList pos)

testKjor = do
    --input <- getLine
    --let pos = tail input
    --putStr (pos ++ "\n")
    --let tall = strToIntList pos
    --putStr ((show tall) ++ "\n" ++ (show (length tall) ++ "\n"))
    --let posisjoner = posList tall
    --mapM_ (\x -> putStr (show x ++ " ")) posisjoner
    --putStr "\n"
    --let levendeCeller = n posisjoner
    --showCeller levendeCeller
    mapM_ (\y -> putStr (show (getCellePos y) ++ ", ")) (getLevendeCeller cell)

regl :: Regel
regl = (Regel (3,3) (2,3))

brtt :: Board
brtt = [(Celle (1,1) False), (Celle (1,2) False), (Celle (1,3) False), (Celle (2,1) True), (Celle (2,2) True), (Celle (2,3) True), (Celle (3,1) False), (Celle (3,2) False), (Celle (3,3) False)]

cee :: Celle 
cee = (Celle (3,2) False)
see :: Celle
see = (Celle (2,2) True)

testNxtGn :: IO ()
testNxtGn = do 
    showCeller brtt
    putStr "\n"
    --putStr ("antLevendeNabo: " ++ (show (antLevendeNabo see brtt)) ++ "\n")
    --putStr "naboer: "
    --showCeller (visNaboer cee brtt)
    putStr ("\n overlever:\n") 
    showCeller (overlever regl brtt brtt)
    putStr "\n fodt:\n"
    showCeller (born regl brtt brtt)
    putStr "\nnesteGen\n"
    let survBrett = overlever regl brtt brtt
    let bornBrett = born regl brtt brtt
    let nesteCeller = survBrett ++ bornBrett
    let nextGen = oppdaterBrett nesteCeller brtt
    showCeller nextGen
    --visOverlever regl brtt

merge :: [Celle] -> [Celle] -> [Celle]
merge surv born = surv ++ born

visNaboer :: Celle -> Board -> [Celle]    
visNaboer c b = do 
    let (x,y) = getCellePos c
    let naboliste = [(x-1, y-1), (x,y-1),(x+1, y-1),(x-1, y),(x+1, y),(x-1, y+1),(x,y+1),(x+1, y+1)]
    --let hentedeCeller = 
    getCellerFraBrett b naboliste
    --map (\a -> isAliveInt a) hentedeCeller
ovr :: Bool
ovr = ((antLevendeNabo see brtt) >= (fst (getS regl)) && (antLevendeNabo see brtt) <= (snd (getS regl)))
brn :: Bool
brn = ((antLevendeNabo cee brtt) >= (fst (getB regl)) && (antLevendeNabo cee brtt) <= (snd (getB regl))) 
realSimple :: Regel -> Celle -> Board -> Int
--realSimple _ [] = []
realSimple reg c b = 
    if ((isAlive c) && (antLevendeNabo c b) >= (fst (getS reg)) && (antLevendeNabo c b) <= (snd (getS reg))) 
        then 1 
    else 0 