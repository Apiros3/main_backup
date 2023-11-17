{-# LANGUAGE BangPatterns #-}

-- 11.1
foldB :: (Bool -> a) -> (Bool -> a) -> Bool -> a
foldB l r True  = l True 
foldB l r False = r False

idB :: Bool -> Bool 
idB = foldB (\x -> True) (\x -> False)

data Day =  Sunday | Monday | Tuesday | Wednesday |
            Thursday | Friday | Saturday
            deriving (Show)

foldD :: (Day -> a) -> (Day -> a) -> (Day -> a) -> (Day -> a) -> (Day -> a) -> (Day -> a) -> (Day -> a) -> Day -> a 
foldD f _ _ _ _ _ _ Monday      = f Monday 
foldD _ f _ _ _ _ _ Tuesday     = f Tuesday 
foldD _ _ f _ _ _ _ Wednesday   = f Wednesday 
foldD _ _ _ f _ _ _ Thursday    = f Thursday 
foldD _ _ _ _ f _ _ Friday      = f Friday 
foldD _ _ _ _ _ f _ Saturday    = f Saturday 
foldD _ _ _ _ _ _ f Sunday      = f Sunday 

idD :: Day -> Day 
idD = foldD (\x -> Monday) (\x -> Tuesday) (\x -> Wednesday) (\x -> Thursday) (\x -> Friday) (\x -> Saturday) (\x -> Sunday)

-- 11.3
data Set a = Empty | Singleton a | Union (Set a) (Set a)

foldS :: b -> (Set a -> b) -> (b -> b -> b) -> Set a -> b 
foldS u v w Empty           = u 
foldS u v w (Singleton p)   = v (Singleton p)
foldS u v w (Union p q)     = w (foldS u v w p) (foldS u v w q) 

isIn :: Eq a => a -> Set a -> Bool 
isIn p = foldS False (\(Singleton u) -> u == p) (||)

subset :: Eq a => Set a -> Set a -> Bool 
subset p q = foldr (&&) (True) mpF 
    where mpF = map (\x -> isIn x q) (foldS ([]) (\(Singleton x)-> [x]) (++) p)

equals :: Eq a => Set a -> Set a -> Bool 
equals p q = (subset p q) && (subset q p)

-- 11.4
data BTree a = Leaf a | Fork (BTree a) (BTree a) deriving (Show)
data Direction = L | R deriving (Show) 
type Path = [Direction]

foldT :: (BTree a -> b) -> (b -> b -> b) -> BTree a -> b 
foldT l r (Leaf a)      = l (Leaf a)
foldT l r (Fork p q)    = r (foldT l r p) (foldT l r q)

find :: Eq a => a -> BTree a -> Maybe Path 
find p q = foldT (\(Leaf x) -> if x == p then Just [] else Nothing) (f) q
    where   f Nothing Nothing   = Nothing 
            f Nothing (Just p)  = Just (R:p)
            f (Just p) _        = Just (L:p)

-- 12.1
data Queue a = Que' [a]

empty :: Queue a 
empty = Que' []

isEmpty :: Queue a -> Bool 
isEmpty (Que' [])   = True 
isEmpty _           = False 

add :: a -> Queue a -> Queue a 
add p (Que' q)  = Que' (p:q) 

get :: Queue a -> (a, Queue a)
get (Que' [])       = error "List is empty"
get (Que' p)        = (x, Que' (reverse xs))
    where (x:xs) = reverse p 

-- data Queue = Que Front Back
data Queue' a = Que [a] [a] deriving (Show)

empty' :: Queue' a 
empty' = Que [] []

isEmpty' :: Queue' a -> Bool 
isEmpty' (Que [] [])    = True 
isEmpty' _              = False 

add' :: a -> Queue' a -> Queue' a 
add' p (Que xs ys) = Que (p:xs) ys 

get' :: Queue' a -> (a, Queue' a)
get' (Que [] [])        = error "Queue is Empty"
get' (Que xs (y:ys))    = (y, Que xs ys)
get' (Que xs _)         = (t, Que [] ts)
        where (t:ts) = reverse xs

-- 12.2
fib :: Integer -> Integer 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- ----------------
-- *Main> fib 10
-- 55
-- (0.00 secs, 114,304 bytes)
-- *Main> fib 20
-- 6765
-- (0.04 secs, 7,549,120 bytes)
-- *Main> fib 30
-- 832040
-- (4.32 secs, 921,578,856 bytes)
--  ----------------

two :: Integer -> (Integer,Integer)
two 0 = (0,1)
two n = (snd t,uncurry (+) t)
    where t = two (n-1)

--  ----------------
-- *Main> two 10
-- (55,89)
-- (0.01 secs, 67,408 bytes)
-- *Main> two 20
-- (6765,10946)
-- (0.00 secs, 80,352 bytes)
-- *Main> two 30
-- (832040,1346269)
-- (0.01 secs, 91,744 bytes)
--  ----------------

roughly :: Integer -> String 
roughly n = x : 'e' : show (length xs)
    where (x:xs) = show n 

-- *Main> (roughly.fst.two) 10000
-- "3e2089"
-- (0.02 secs, 9,165,376 bytes)  

power :: Num a => (a,a,a,a) -> (a,a,a,a) -> Int -> (a,a,a,a)
power y x n     | n == 0    = y
                | even n    = power y (f x x) (n `div` 2)
                | odd n     = power (f x y) x (n-1)
    where f (a,b,c,d) (e,f,g,h) = (a*e+b*g,a*f+b*h,c*e+d*g,c*f+d*h)

fib2 :: Int -> Integer
fib2 = t.power (1,0,0,1) (0,1,1,1)
    where t (_, x, _, _) = x 

-- *Main> (roughly.fib2) 1000000
-- "1e208987"
-- (0.06 secs, 14,201,056 bytes)

-- 12.4
test :: ((b -> [Char] -> a) -> () -> [[Char]] -> t) -> t
test f = f (const error) () ["strict","lazy"]

loop :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
loop s n []     = n 
loop s n (x:xs) = loop s (s n x) xs

loop' :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
loop' s n []        = n 
loop' s (!n) (x:xs) = loop' s (s n x) xs    