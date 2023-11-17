

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs 

curry' :: ((a,b) -> c) -> a -> b -> c 
curry' f a b = f (a,b)

uncurry' :: (a -> b -> c) -> (a,b) -> c 
uncurry' f (a,b) = f a b 


pascal :: [[Integer]]
pascal = iterate f [1]
    where f x = zipWith (+) (x ++ [0]) ([0] ++ x)


foldl' :: (b -> a -> b) -> b -> [a] -> b 
foldl' f e [] = e 
foldl' f e (x:xs) = foldl' f (f e x) xs 


type Word' = String 
type Text = [Word']

-- c = count.toLower 
count :: Text -> [(Word',Int)]
count [] = []
count (x:xs) = (x, cnt) : count ys 
    where cnt = length (filter (==x) xs) + 1 
          ys = filter (/=x) xs 


sort :: [(Word',Int)] -> [(Word',Int)]
sort [] = []
sort [x] = [x]
sort xs = merge (sort ys) (sort zs)
    where (ys,zs) = splitAt (length xs `div` 2) xs 
          merge ((a,b):xs) ((c,d):ys)
                | b > d || (b == d && a < c) = (a,b) : merge xs ((c,d):ys)
                | otherwise = (c,d) : merge ((a,b):xs) ys 
          merge x [] = x 
          merge [] y = y 



data Tree a = Nil | Fork a (Tree a) (Tree a)

insert :: Ord a => a -> Tree a -> Tree a 
insert x Nil = Fork x Nil Nil 
insert x (Fork a b c)
    | x < a = Fork a (insert x b) c 
    | x > a = Fork a b (insert x c)
    | otherwise = Fork a b c 


foldSTree :: (a -> b -> b -> b) -> b -> Tree a -> b 
foldSTree f e Nil = e 
foldSTree f e (Fork a b c) = f a (foldSTree f e b) (foldSTree f e c)


flatten :: Tree a -> [a]
flatten = foldSTree f []
    where f a b c = b ++ [a] ++ c 

size :: Tree a -> Int 
size = foldSTree f 0 
    where f a b c = 1 + b + c 

depth :: Tree a -> Int 
depth = foldSTree f 0 
    where f a b c = 1 + (b `max` c)

delete :: Ord a => a -> Tree a -> Tree a 
delete _ Nil = Nil 
delete x (Fork a b c)
    | x < a = Fork a (delete x b) c 
    | x > a = Fork a b (delete x c)
    | otherwise = f b c 
        where f b Nil = b 
              f b (Fork x y z) = Fork x b (f y z)
    
