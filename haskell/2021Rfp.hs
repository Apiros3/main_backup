

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] ++ [y | y <- (x:xs), y == x] ++ sort [y | y <- xs, y > x]


data Tree a = Nil | Fork a Int (Tree a) (Tree a)

toTree' :: Ord a => [a] -> Tree a
toTree' [] = Nil
toTree' (x:xs) = Fork x (length [y | y <- x:xs, y == x]) (toTree' [y | y <- xs, y < x]) (toTree' [y | y <- xs, y > x])

fromTree' :: Tree a -> [a]
fromTree' Nil = []
fromTree' (Fork x k left right) = fromTree' left ++ f x k ++ fromTree' right 
    where f x 0 = []
          f x n = x : f x (n-1)
    
sort'' :: Ord a => [a] -> [a]
sort'' = fromTree'.toTree'

fold :: (a -> Int -> b -> b -> b) -> b -> Tree a -> b
fold f e Nil = e 
fold f e (Fork x k left right) = f x k (fold f e left) (fold f e right)

fromTree :: Tree a -> [a]
fromTree = fold f [] 
    where f x k l r = l ++ rep x k ++ r 

rep :: a -> Int -> [a]
rep x 0 = []
rep x n = x : rep x (n-1)

unfold :: (b -> Maybe(a,Int,b,b)) -> b -> Tree a 
unfold p x = case p x of 
    Nothing -> Nil 
    Just(x,cnt,left,right) -> Fork x cnt (unfold p left) (unfold p right)

toTree :: Ord a => [a] -> Tree a 
toTree = unfold p 
    where p [] = Nothing 
          p (x:xs) = Just(x,len,left,right)
            where len = length [y | y <- (x:xs), y == x]
                  left = [y | y <- xs, y < x]
                  right = [y | y <- xs, y > x]

sort' :: Ord a => [a] -> [a]
sort' = fromTree.toTree 


type Square = [[Int]]

cp :: [[a]] -> [[a]]
cp = foldr f [[]] 
    where f xs yss = [x:ys | x <- xs, ys <- yss] 

squares :: Int -> [Square]
squares n = cp (f n (cp (f n [1..n])))
    where f 0 _ = [] 
          f n x = x : f (n-1) x 

latin :: Int -> [Square]
latin n = (filter valid . squares) n  
    where valid x = fn x n && fn (transpose x) n 

fn xs n = foldr (&&) True (map g xs)
    where g xs = (sort xs == [1..n])

transpose :: Square -> Square 
transpose [x] = [[y] | y <- x]
transpose (x:xs) = zipWith (:) x (transpose xs)


type List a = Int -> Maybe a 

nil' :: List a 
nil' = const Nothing 

cons' :: a -> List a -> List a 
cons' x f = g 
    where g 0 = Just x  
          g n = f (n-1)

list' :: [a] -> List a 
list' = foldr cons' nil' 

null' :: List a -> Bool 
null' f = case f 0 of 
    Nothing -> True 
    _       -> False 

head' :: List a -> a 
head' f = (\(Just a) -> a) (f 0)

tail' :: List a -> List a 
tail' f = g 
    where g x = f (x+1)

map' :: (a -> b) -> List a -> List b 
map' f g = h 
    where h x = k (g x)
          k Nothing = Nothing 
          k (Just z) = Just (f z) 

take' :: Int -> List a -> List a 
take' x f = g 
    where g y 
            | y < x = f x 
            | otherwise = Nothing 

drop' :: Int -> List a -> List a 
drop' x f = g 
    where g y 
            | y < x = Nothing 
            | otherwise = f (y - x)

length' :: List a -> Int 
length' f = case f 0 of 
    Nothing -> 0 
    Just _  -> 1 + length' (tail' f)


ujust :: Maybe a -> a 
ujust (Just x) = x 

ssort :: Ord a => List a -> List a 
ssort f 
    | null' f  = nil' 
    | otherwise = merge (ssort l) (ssort r)
          where l = take' (length' f `div` 2) f 
                r = drop' (length' f `div` 2) f
                merge g h 
                    | null' g = h 
                    | null' h = g 
                    | g 0 <= h 0 = cons' (ujust (g 0)) (merge (tail' g) h)
                    | otherwise = cons' (ujust (h 0)) (merge g (tail' h))       


