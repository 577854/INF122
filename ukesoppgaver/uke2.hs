
-- B --
-- Oppgave 3.3 fra boken
second :: [a] -> a --type
second xs = head (tail xs)

swap :: (b,a) -> (a,b) --type
swap (x,y) = (y,x)

pair :: a -> b -> (a,b) -- type
pair x y = (x,y)

double :: (Num a) => a -> a --type
double x = x*2 

palidrome :: (Eq a) => [a] -> Bool --type
palidrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t -- type
twice f x = f (f x)


-- C -- 
{- 
False :: Bool
5 + 8 :: Num a => a
(+) 2 :: Num a => a -> a
(+2) :: Num a => a -> a
(2+) :: Num a => a -> a 
(["foo", "bar"], 'a') :: ([[Char]], Char)
[(True, []), (False, [['a']])] :: [(Bool, [[Char]])]
\x y -> y !! x :: Int [a] -> a
[take, drop, \x y -> (y !! x)] :: error, (y !! x) må være av typen [] fordi at listen må inneholde samme type. 
[take, drop, \x y -> [ y !! x ]] :: [Int -> [a] -> [a]] 
-}

-- D --
foo1 :: a -> b -> (a, b)
foo1 x y = (x, y)

foo2 :: a -> b -> (a, b)
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a, b)
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a, b)
foo4 = \x -> \y -> (x, y)

foo5 :: a -> b -> (b, a)
foo5 = \x -> \y -> (y, x)

foo6 :: a -> b -> (a, b)
foo6 = \y -> \x -> (y, x)

{-
alle har samme argument (to typer) og samme output (tuppel bestående av to typer)
foo1 == foo2 == foo3 == foo4 == foo6

foo5 tar også to argument, men gir tilbake et tuppel bestående av 2 typer som er reversert.
første argument er sist og andre argument er først i tuppelet
-}

-- E --
f1 :: a -> (a,a)
f1 x = (x,x)

f2 :: (a,b) -> a
f2 (x,y) = x

f3 :: (a,b) -> b
f3 (x,y) = y

f4 :: a -> b -> a
f4 x y = x

f5 :: a -> b -> b
f5 x y = y

-- F --
f :: Int -> Int -> Int
f x y = x + y

g :: (Int, Int) -> Int
g (x,y) = x + y
