import Data.Char (isDigit, isLetter, isAlpha)

-- 1 --
data AST = V Int | W String | P AST AST | M AST AST deriving (Show)
    -- V blad tall, W blad word, O ord, P pluss, M multiplikasjon

eval :: AST -> Int
eval (V n) = n
eval (P x y) = eval x + eval y 
eval (M x y) = eval x * eval y
eval (W x) = error "kunne ikke håndtere ord i et aritmetisk utrykk"

-- 2 --
inn :: AST -> String 
inn (V n) = show n
inn (P x y) = inn' x ++ " + " ++ inn' y
inn (M x y) = inn' x ++ " * " ++ inn' y
inn n = inn' n

-- fra lab (hjelpefunksjon for parantes slik at de ikke kommer ytterst i tilegg):
-- ble feil lol fikser senere
inn' :: AST -> String
inn' (V n) = show n
inn' (W x) = x
inn' (P x y) = "(" ++ inn' x ++ " + " ++ inn' y ++ ")"
inn' (M x y) = "(" ++ inn' x ++ " * " ++ inn' y ++ ")"

-- 3.1 --
-- mulig å bruke isAlpha istedenfor isLetter. kan kansje legge til error i otherwise da?
tokenize :: String -> [String]
tokenize [] = []
tokenize (x:' ':xs) = [x] : tokenize xs
tokenize (x:xs) 
    | x `elem` "+*" = [x] : tokenize xs
    | isDigit x = takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit xs)
    | isLetter x = takeWhile isLetter (x:xs) : tokenize (dropWhile isLetter xs)
    | otherwise = tokenize xs

-- 3.2 --
-- fikk ikke til å parse tekst men tall skal fungere å parse til AST
parse :: String -> AST
parse str = fst (parseTokens x)
    where x = tokenize str

parse' :: [String] -> (AST, [String])
parse' ("+":xs) = 
    let 
        (ast1, rest1) = parse' xs;
        (ast2, rest2) = parse' rest1 
    in (P ast1 ast2, rest2)

parse' ("*":xs) = 
    let (ast1, rest1) = parse' xs;
        (ast2, rest2) = parse' rest1 
    in (M ast1 ast2, rest2)

parse' (x:xs)
    | all isDigit x = (V (read x), xs)
    | all isAlpha x = (W x, xs) 
parse' _ = error "noe gikk galt.kunne ikke parse utrykk"

onlyDigit :: [Char] -> Bool
onlyDigit x = takeWhile isDigit x == x

onlyLetter :: [Char] -> Bool
onlyLetter x = takeWhile isLetter x == x 

parseTokens :: [String] -> (AST, [String])
parseTokens ("*":xs) =
    let
        (ast1, rest1) = parseTokens xs
        (ast2, rest2) = parseTokens rest1
    in (M ast1 ast2, rest2)
parseTokens ("+":xs) =
    let
        (ast1, rest1) = parseTokens xs
        (ast2, rest2) = parseTokens rest1
    in (P ast1 ast2, rest2)
parseTokens (x:xs)
    | all isDigit x = (V (read x), xs)
    | all isAlpha x = (W x, xs)
parseTokens _ = error "Could not parse invalid expression"

-- 3.3 --
--ev :: String -> Int
--ev a = eval x
--    where x = parse a

-- dette var en veldig utfordrende ukesoppgave
-- brukte kansje for mye tid på 3.2

-- 3.3 --
-- har faktisk gjort oppgaven?
ev :: String -> Int
ev str = eval (parse str)

-- 3.4 --
innfiks :: String -> String
innfiks str = inn (parse str)