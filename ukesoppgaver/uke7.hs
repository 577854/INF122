-- A --

-- 7.1
-- [f x | x <- xs, p x]
-- map f (filter p xs)

-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0 

-- 7.5
myCurry :: ((a, b) -> c) ->  (a -> b -> c)
myCurry f = \x y -> f (x, y)
myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(x, y) -> f x y

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
--altMap f _ [x] = [f x] -- la til denne under gjennomgang
--altMap f g (x:y:xs) = [f x, g y] ++ altMap f g xs
altMap f g (x:xs) = f x : altMap g f xs -- <-alternativ

-- 8.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val a) = f a 
folde f g (Add v h) = g (folde f g v) (folde f g h)

-- 8.6
eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size e = folde (const 1) (+) e

-- B --
infiks :: Expr -> String
infiks (Val a) = show a
infiks (Add v h) = infiks' v ++ " + " ++ infiks' h

infiks' :: Expr -> String
infiks' x = folde show (\y z -> "(" ++ y ++ " + " ++ z ++ ")") x

prefiks :: Expr -> String
prefiks (Val a) = show a
--prefiks (Add v h) = "+ " ++ prefiks v ++ " " ++ prefiks h 
prefiks x = folde show (\y z -> "+ " ++ y ++ z) x

postfiks :: Expr -> String
postfiks (Val a) = show a
--postfiks (Add v h) = postfiks v ++ " " ++ postfiks h ++ " +"
postfiks x = folde show (\y z -> y ++ z ++ " +") x