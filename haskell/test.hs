

f :: Int -> [[Int]]
f 1 = [[1]]
f n = (g (z ++ [0], [0] ++ z)) : (f (n-1))
    where z :: [Int]
          z = head (f (n-1))
          g :: ([Int],[Int]) -> [Int] 
          g ([],[]) = []
          g ((x:xs),(y:ys)) = (x+y) : (g (xs,ys)) 

g :: [Int] -> [Int]
g x = h (x ++ [0]) ([0] ++ x)
    where h :: [Int] -> [Int] -> [Int]
          h [] [] = []
          h (x:xs) (y:ys) = (x+y) : (h xs ys)

pascal :: [[Int]]
pascal = iterate g [1]


h :: Int -> Int 
h n = case n of 
    0 -> 0
    n -> n + h (n-1)

comp :: (b -> c) -> (a -> b) -> a -> c 
comp x y z = x (y z)

flip2 :: (a -> b -> c) -> b -> a -> c 
flip2 x y z = x z y 

dup :: (a -> a -> b) -> a -> b 
dup x y = x y y 


alts :: [a] -> ([a], [a])
alts [] = ([],[])
alts (x:xs) = (x:u,v)
    where (v,u) = alts xs 


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f x y = map (uncurry f) (zip x y)
zipWith' f x y = foldr ((:). uncurry f) [] (zip x y)

splits :: [a] -> [(a,[a])]
splits xs = unfold f g h ([],xs)  
    where f (_,[]) = True 
          f _ = False 
          g (xs,ys) = (head ys, xs ++ tail ys)
          h (xs,y:ys) = (xs ++ [y],ys)

unfold :: (b->Bool) -> (b->a) -> (b->b) -> b -> [a]
unfold null head tail = u
    where u x = if null x then [] else head x : u (tail x)

permutation [] = [[]]
permutation xs = [x :zs | (x,ys) <- splits xs, zs <- permutation ys]

pmt :: [a] -> [[a]]
pmt = foldr (\x xs -> [zs | ys <- xs, zs <- inc x ys]) [[]]

inc :: a -> [a] -> [[a]]
inc x = foldr (\y ys -> (x:y:tail (head ys)) : map (y:) ys) [[x]]

include x [] = [[x]]
include x (y:ys) = (x:y:ys) : map (y:) (include x ys)



-- qsort :: [a] -> [a]
-- qsort = flatten.build 

data QTree a = Empty | Branch (QTree a) (QTree a)

-- build' :: Ord a => QTree a -> [a]


-- flatten' :: [a] -> QTree a 



data MTree a = Nil | Node a | Fork (MTree a) (MTree a) deriving(Show)

foldM :: b -> (a -> b) -> (b -> b -> b) -> MTree a -> b
foldM f g h Nil = f 
foldM f g h (Node x) = g x 
foldM f g h (Fork x y) = h (foldM f g h x) (foldM f g h y)

data Three = Zero | One | Mul 
unfoldM :: (b -> Three) -> (b -> a) -> (b -> (b,b)) -> b -> MTree a 
unfoldM f g h x = case f x of 
    Zero -> Nil 
    One -> Node (g x)
    Mul -> Fork (unfoldM f g h u) (unfoldM f g h v)
        where (u,v) = h x 

flattenM :: Ord a => MTree a -> [a]
flattenM = foldM [] (\x -> [x]) merge 
    where merge [] x = x 
          merge x [] = x 
          merge (x:xs) (y:ys) = if x < y then x:merge xs (y:ys) else y:merge (x:xs) ys

buildM :: [a] -> MTree a 
buildM = unfoldM f g h
    where f [] = Zero 
          f [x] = One 
          f _ = Mul
          g [x] = x  
          h xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort = flattenM.buildM





