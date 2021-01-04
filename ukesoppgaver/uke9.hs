import System.IO ()

-- A --
brett nR = if nR <= 0 || nR > 100 then error "matrisen kan ikke være negativ eller større enn 100x100" 
    else 
        do 
            clear
            forsteLinje nR
            mapM_ (\i -> nLinje i nR) [1..nR]
            putStr "\ESC7" -- save position
            func nR
-- B --
func nR = do
    input <- getLine
    let inputStr = concat (words input)
    if nR <= 9 then 
        if (head inputStr) == 'n' then do 
            skrivX ((((read ([inputStr !! 1])::Int) * 2)+2), ((read ([inputStr !! 2])::Int)+1)) 
            func nR
        else if (head inputStr) == 'd' then do
            skrivDot ((((read ([inputStr !! 1])::Int) * 2)+2), ((read ([inputStr !! 2])::Int)+1)) 
            func nR
        else if inputStr == "q" then
            return () 
        else do print "Ukjent kommando"
    else if nR > 9 then
        if (head inputStr) == 'n' then 
            if (length inputStr) > 3 then do 
                skrivX (((read ([inputStr !! 1] ++ [inputStr !! 2])::Int)*3)+2, (read ([inputStr !! 1] ++ [inputStr !! 2])::Int)+1) 
                func nR
            else do
                skrivX ((((read ([inputStr !! 1])::Int) * 3)+2), ((read ([inputStr !! 2])::Int)+1)) 
                func nR
        else if (head inputStr) == 'd' then 
            if (length inputStr > 3) then do
                skrivDot (((read ([inputStr !! 1] ++ [inputStr !! 2])::Int)*3)+2, (read ([inputStr !! 1] ++ [inputStr !! 2])::Int)+1)
                func nR
            else do
                skrivDot ((((read ([inputStr !! 1])::Int) * 3)+2), ((read ([inputStr !! 2])::Int)+1)) 
                func nR
        else if inputStr == "q" then
            return ()
        else do print "Ukjent kommando"
    else do print "Ukjent kommando"

--func2 n = do
--    input <- getLine
--    let inputStr = concat (words input)


forsteLinje :: Int -> IO ()
forsteLinje n = if n <= 9 then writeAt (ve + 1, 0) (concat [(show i) ++ " " | i <- [1..n]] ++ "\n") 
                else writeAt (ve + 2, 0) (concat ([(show i) ++ "  " | i <- [1..9]] ++ [(show i) ++ " " | i <- [10..n]]) ++ "\n") 

nLinje :: Int -> Int -> IO ()
nLinje i n = do
    writeAt (if i > 9 then (ve - 2) else (ve - 1), 1+i) (show i)
    if n <= 9 then mapM_ (\i -> putStr " .") [1..n] else mapM_ (\i -> putStr "  .") [1..n]
    putStrLn ""

-- hjelpefunksjoner --

skrivX :: (Int,Int) -> IO ()
skrivX (x,y) = do 
    writeAt (x,y) "X" 
    putStr "\ESC8" -- restore position

skrivDot :: (Int,Int) -> IO ()
skrivDot (x,y) = do
    writeAt (x,y) "."
    putStr "\ESC8" -- restore position

writeAt :: (Show a1, Show a2) => (a1, a2) -> String -> IO ()
writeAt (x,y) xs = do 
    goto y x
    putStr xs

goto :: (Show a1, Show a2) => a1 -> a2 -> IO ()
goto x y = putStr ("\ESC["++ show x ++ ";"++ show y ++ "H")

ve = 3

clear :: IO ()
clear = putStr "\ESC[2J"

-- lab --

-- gjør func bedre, kortere, del opp
-- bare bruk write at til å skrive! trenger ikke skrivX eller skrivDot 