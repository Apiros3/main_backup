import Data.List () 



-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f = foldr g (const [])
--     where g x = case g x \y of 


-- zipWith' f = somefunc 
--     where somefunc [] = const []
--           somefunc (x:xs) = \list -> case list of 
--                 [] -> []
--                 (y:ys) -> f x y : somefunc xs ys

-- zipWith' f = foldr g (const [])
--     where g x = \list -> case list of 
--         [] -> []
--         (y:ys) -> f x y : 

mergeall :: Ord a => [[a]] -> [a]
mergeall = foldr f []
    where f x [] = x 
          f [] y = y
          f (x:xs) (y:ys) 
            | x < y     = x : f xs (y:ys)
            | otherwise = y : f (x:xs) ys 

mergeall' :: Ord a => [[a]] -> [a]
-- mergeall' [[]] = []
mergeall' [x] = x
mergeall' xs = merge (mergeall' u) (mergeall' v)  
    where (u,v) = splitAt (length xs `div` 2) xs 
          merge x [] = x 
          merge [] y = y 
          merge (x:xs) (y:ys) 
              | x < y   =  x : merge xs (y:ys)
              | otherwise = y : merge (x:xs) ys

runs :: Ord a => [a] -> [[a]]
runs = foldr f [[]]
    where f x [[]] = [[x]]
          f x xss 
            | x <= head (head xss) = (x:head xss):tail xss
            | otherwise            = [x] : xss 




data Tree a b = Tree a b [Tree a b]

bfs :: Eq a => a -> Tree a b -> [b]
bfs x (Tree a b c) = g [Tree a b c]
    where g [] = []
          g (Tree m n z : zs) 
            | m == x = n : g (zs ++ z)
            | otherwise = g (zs ++ z)

exampleT :: Tree Int Int 
exampleT = Tree 1 0 [Tree 1 1 [Tree 1 3 [], Tree 1 4 []], Tree 1 2 [Tree 1 5 [], Tree 1 6 []]]

expand :: (a -> [a]) -> a -> Tree a [a]
expand f x = Tree x (f x) (map (expand f) (f x))

adj :: Int -> [Int]
adj m = [2*m+1,2*m+2]

-- foldr f g [] = g 
-- foldr f g (x:xs) = f x (foldr f g xs)

adjacent :: Eq a => [a] -> [a] -> Bool
adjacent x y = sum (zipWith (\x y -> if x == y then 0 else 1) x y) == 1


