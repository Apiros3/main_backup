import Graphics.Win32.GDI.Font (aNSI_VAR_FONT)
import Data.List (unfoldr, uncons)


plus :: Int -> Int -> Int
plus a b = a+b

-- 5.1
take2 :: Int -> [a] -> [a]
take2 _ []      = []
take2 0 _       = []
take2 n (x:xs)  = x:take2 (n-1) xs

drop2 :: Int -> [a] -> [a]
drop2 _ []      = []
drop2 0 xs      = xs
drop2 n (x:xs)  = drop2 (n-1) xs

-- 5.3
evens2 :: [a] -> [a]
evens2 []           = []
evens2 (x1:[])      = [x1]
evens2 (x1:x2:xs)   = x1:evens2 xs

odds2 :: [a] -> [a]
odds2 []            = []
odds2 (x:xs)        = evens2 xs

alts :: [a] -> ([a],[a])
alts []             = ([],[])
alts (x:xs)         = (x:ev,od)
    where (od,ev) = alts xs

-- 6.1
zip2 :: [a] -> [b] -> [(a,b)]
zip2 [] _           = []
zip2 _ []           = []
zip2 (x:xs) (y:ys)  = (x,y):zip2 xs ys

-- -- 6.2
zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 f x y = [f x2 y2 | (x2,y2) <- zip2 x y]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys)    = f x y : zipWith f xs ys
zipWith' f _ _              = []

zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith' f
    where   f :: a -> b -> (a,b)
            f a b = (a,b)

-- 6.5
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (x:xs) = 
    [zs | ys <- permutations' xs, zs <- include' x ys]

include' :: a -> [a] -> [[a]]
include' x [] = [[x]]
include' x (y:ys) = (x:y:ys) : map (y:) (include' x ys)

permutations2 :: [a] -> [[a]]
permutations2 = foldr (concatMap . include2) [[]]

include2 :: a -> [a] -> [[a]]
include2 x = foldr(\z y -> (x:z:tail(head y)):map (z:) y) [[x]]

-- 6.7
uncons :: [a] -> Maybe (a, [a])
uncons    []  = Nothing
uncons (x:xs) = Just (x,xs)

delete:: Eq a => a -> [a] -> [a]
delete y xs = 
        takeWhile (/= y) xs ++ tail (dropWhile (/= y) xs)

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = y : ssort ys
    where y = minimum xs
          ys = delete (minimum xs) xs 

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold null head tail = u 
    where u x = if null x then [] else head x : u (tail x)

ssort2 :: Ord a => [a] -> [a]
ssort2 = unfold (==[]) minimum deleteMin 
    where deleteMin xs = delete (minimum xs) xs 

-- unfoldr2 :: (a -> Maybe (b, a)) -> a -> [b]
-- unfoldr2 foo = f 
--     where f x = if foo x then [] else 

ssort3 :: Ord a => [a] -> [a]
ssort3 = unfoldr (\ys -> if null ys then Nothing else Just (minimum ys, deleteMin ys))
    where deleteMin ys = delete (minimum ys) ys 
    
