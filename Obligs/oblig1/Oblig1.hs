-- Gudsteinn Arnarson, group 3
module Oblig1 where
import Data.Char

data Ast = Word String | Num Int | Mult Ast Ast | Plus Ast Ast | Minus Ast Ast deriving (Eq, Show)

-- 1 --
---------------------------------------------------------------------------------------------
parse :: String -> Ast
parse str = fst (parseTokens x)
    where x = tokenize str

parseTokens :: [String] -> (Ast, [String])
-- Mult operator
parseTokens (x:"*":y:xs) = -- Number * Term 
    if (isDigit (x !! 0) && isAlpha (y !! 0)) || (isDigit (x !! 0) && isDigit (y !! 0) && (xs !! 0) == "*") then 
        let 
            (ast1, rest1) = parseTokens (x:y:xs);
            (ast2, rest2) = parseTokens rest1
        in (Mult ast1 ast2, rest1)
    else error "parse error: ord foran * eller tall bak * operatøren uten ord bak. ugyldig utrykk!"

-- plus operator
parseTokens (x:"+":y:xs) = -- Term + Expr
    if (isAlpha (x !! 0)) && length xs > 0 then
        if ((isDigit (y !! 0) && (xs !! 0) == "*")) || (isAlpha (y !! 0) && (xs !! 0) /= "*") then
            let
                (ast1, rest1) = parseTokens (x:y:xs);
                (ast2, rest2) = parseTokens rest1
            in (Plus ast1 ast2, rest2)
        else error "parse error: ugyldig utrykk med + operatøren. ugyldig utrykk!"
    else 
        if isAlpha (y !! 0) then 
            let
                (ast1, rest1) = parseTokens (x:y:xs);
                (ast2, rest2) = parseTokens rest1
            in (Plus ast1 ast2, rest2)
        else 
            error "parse error: feil tegn i + operasjonen. ugyldig utrykk!"

-- Minus operator
parseTokens (x:"-":y:xs) = -- Term - Expr
    if (isAlpha (x !! 0)) && length xs > 0 then
        if ((isDigit (y !! 0) && (xs !! 0) == "*")) || (isAlpha (y !! 0) && (xs !! 0) /= "*") then
        let 
            (ast1, rest1) = parseTokens (x:y:xs);
            (ast2, rest2) = parseTokens (rest1)
        in (Minus ast1 ast2, rest2)
        else error "parse error: ugyldig utrykk med - operatøren. ugyldig utrykk!"
    else 
        if isAlpha (y !! 0) then 
            let
            (ast1, rest1) = parseTokens (x:y:xs);
            (ast2, rest2) = parseTokens rest1
        in (Minus ast1 ast2, rest2)
    else 
        error "parse error: feil tegn i - operasjonen. ugyldig utrykk!"

-- for alle andre tilfeller
parseTokens (x:xs) = -- Number eller Word
    if (tail (x:xs)) /= ["*"] && (tail (x:xs)) /= ["+"] && (tail (x:xs)) /= ["-"] then
        if all isDigit x then (Num (read x), xs)
        else if all isAlpha x then (Word x, xs)
        else error "ugyldige tegn brukt i utrykket!"
    else error "+ - eller * er etterfulgt av ingenting. ugyldig utrykk!" 

-- ekstra tilfelle hvis noe går galt. bare for å være sikker
parseTokens _ = error "kunne ikke parse!"

-- tokeniser en string slik at grammatikken blir delt inn i en liste av strings
tokenize :: String -> [String]
tokenize "" = []
tokenize (' ':xs) = tokenize xs
tokenize (x:xs) 
    | x `elem` "*+-" = [x] : tokenize xs
    | isDigit x = takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit (x:xs))
    | isAlpha x = takeWhile isAlpha (x:xs) : tokenize (dropWhile isAlpha (x:xs))
    | otherwise = error ("kunne ikke tokenize token: " ++ show x)
---------------------------------------------------------------------------------------------

-- 2 --
---------------------------------------------------------------------------------------------
viss :: Ast -> String
viss ast = astToString ast 1

vis :: Ast -> IO ()
vis ast = putStr (viss ast)

-- Hjelpemetoder oppg 2 --

-- skriver ut Ast strukturen med denne metoden 
-- ingen feilmeldinger i denne oppgaven fordi det ikke er et krav
-- treet som skriver ut kan skrive med feil grammatikk. eks: vis (Plus (Num 2) (Word "hei")) er gyldig
astToString :: Ast -> Int -> String
astToString (Word n) counter = if counter == 1 then "Word " ++ n else hopp3 (counter-1)  ++ "Word " ++ n  
astToString (Num n) counter = if counter == 1 then "Num " ++ show n else hopp3 (counter-1)  ++ "Num " ++ show n

astToString (Mult x y) counter = 
    if (counter == 0) then "Mult" ++ "\n" ++ acc x counter ++"\n" ++ acc y counter
    else hopp3 (counter-1) ++ "Mult" ++ "\n" ++ acc x counter ++"\n" ++ acc y counter

astToString (Plus x y) counter = 
    if (counter == 0) then "Plus" ++ "\n" ++ acc x counter ++"\n" ++ acc y counter
    else hopp3 (counter-1) ++ "Plus" ++ "\n" ++ acc x counter ++ "\n" ++ acc y counter

astToString (Minus x y) counter = 
    if (counter == 0) then "Minus" ++ "\n" ++ acc x counter ++"\n" ++ acc y counter
    else hopp3 (counter-1) ++ "Minus" ++ "\n" ++ acc x counter ++"\n" ++ acc y counter

-- akumulator/accumulator
acc :: Ast -> Int -> String
acc ast x = astToString ast (x+1) 

-- hopper 3 mellomrom til høyre
hopp3 :: Int -> String
hopp3 0 = ""
hopp3 x = "   " ++ hopp3 (x - 1)
---------------------------------------------------------------------------------------------

-- 3 --
---------------------------------------------------------------------------------------------
eval :: Ast -> String
eval str = evaluate str

evaluate :: Ast -> String
evaluate (Num n) = show n
evaluate (Word n) = n
evaluate (Mult (Num x) (Word a)) = gangerOrd x a
     
evaluate (Mult x y)  
    | all isDigit (evaluate x) && all isAlpha (evaluate y) = gangerOrd (read (evaluate x)) (evaluate y) 
    | otherwise = error "eval error: ikke lov å gange med ord" 

evaluate (Plus x y) 
    | all isAlpha (evaluate x) && all isAlpha (evaluate y) = evaluate x  ++ evaluate y
    | otherwise = error "eval error: ikke lov å bruke tall med operatøren +. ugyldig utrykk!"

evaluate (Minus x y) 
    | all isAlpha (evaluate x) && all isAlpha (evaluate y) = evaluate x `diff` evaluate y
    | otherwise = error "eval error: ikke lov å bruke tall med operatøren -. ugyldig utrykk!"

--evaluate ast = error "eval error: ugyldig utrykk! klarer ikke å evaluere utrykket"

-- differansen mellom en liste fra en annen
diff :: Eq a => [a] -> [a] -> [a]
diff [] [] = []
diff [] as = []
diff as [] = as
diff as (b:bs) = diff (rem1 as b) bs  

-- trekker bokstaver fra venstre side i utrykket 
-- fra bokstavene som er like i høyre side av utrykket
-- eks: minusOrd "abba" "aba" = "b"

-- fjerner en type av a som forekommer 1 gang i liste av type a
-- eks: rem1 "abba" 'b' = "aba"
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) y = if x == y then xs else x : rem1 xs y 

-- gir tilbake en string som er gjentatt x vilkårlige ganger
-- eks: gangerOrd 3 "hei" = "heiheihei"
gangerOrd :: Int -> String -> String
gangerOrd 1 ord = ord
gangerOrd x ord = ord ++ gangerOrd (x - 1) ord 
---------------------------------------------------------------------------------------------