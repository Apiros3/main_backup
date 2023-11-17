import Data.Type.Coercion (trans)




-- Question 2

data RTree a = Node a [RTree a] deriving(Show)

exampleRTree :: RTree Integer 
exampleRTree = 
    Node 1 [
        Node 2 [Node 3 [], Node 4 []],
        Node 5 [],
        Node 6 [Node 7 []]
    ]

foldRTree :: (a -> [b] -> b) -> RTree a -> b 
foldRTree f (Node x xs) = f x [foldRTree f xss | xss <- xs]

perfectRTree :: RTree a -> Bool
perfectRTree t = (foldRTree f t) == -(foldRTree g t)
    where f :: a -> [Int] -> Int 
          f _ [] = 0 
          f _ xs = (mn xs) + 1 
          g :: a -> [Int] -> Int 
          g _ [] = 0 
          g _ xs = (mn xs) - 1  
          mn :: [Int] -> Int 
          mn (x:[]) = x 
          mn (x:xs) = min x (mn xs)


-- Question 3
data Zig a b = Nil | Cins a (Zag b a) deriving(Show)

data Zag a b = Nal | Cans a (Zig b a) deriving(Show)

exampleZig :: Zig Integer Char
exampleZig = Cins 1 (Cans 'A' (Cins 2 Nal))

exampleZag :: Zag String Bool
exampleZag = Cans "C" (Cins True (Cans "D" Nil))

headZig :: Zig a b -> a 
headZig (Cins a _) = a 

headZag :: Zag a b -> a 
headZag (Cans a _) = a 

data ZigOrZagType a b = ZigType a | ZagType b deriving(Show)

lastZig :: Zig a b -> ZigOrZagType a b 
lastZig (Cins a Nal) = ZigType a 
lastZig (Cins _ b) = lastZag b 

lastZag :: Zag a b -> ZigOrZagType b a 
lastZag (Cans a Nil) = ZagType a 
lastZag (Cans _ b) = lastZig b 

mapZig :: (a -> m) -> (b -> n) -> Zig a b -> Zig m n 
mapZig f g Nil = Nil 
mapZig f g (Cins a b) = Cins (f a) (mapZag g f b)

mapZag :: (a -> m) -> (b -> n) -> Zag a b -> Zag m n 
mapZag f g Nal = Nal 
mapZag f g (Cans a b) = Cans (f a) (mapZig g f b)


-- Question 4

type Matrix a = [[a]]

addMat :: Matrix Int -> Matrix Int -> Matrix Int 
addMat x y 
    | (length x == length y) && (length (head x) == length (head y)) = f x y 
    | otherwise =  error "incompatible sizes"
        where f :: Matrix Int -> Matrix Int -> Matrix Int 
              f (m:xs) (n:ys) = g m n : f xs ys 
                    where g (p:ps) (q:qs) = p+q : g ps qs 
                          g _ _ = []
              f _ _ = []

exm :: Matrix Int 
exm = [[1,2,3,4],[5,6,7,8],[9,10,11,12]]


addMat2 :: Num c => [[c]] -> [[c]] -> [[c]]
addMat2 x y = zipWith (zipWith (+)) x y

transpose :: Matrix a -> Matrix a
transpose [] = []
transpose [x] = g x
    where g [] = []
          g (y:ys) = [y]:g ys 
transpose (x:xs) = f x (transpose xs)
    where f (y:ys) (zs:zss) = (y:zs) : f ys zss 
          f _ _ = []

transpose2 :: [[a]] -> [[a]]
transpose2 [x] = zipWith (\y z -> [y]) x x
transpose2 (x:xs) = zipWith (:) x (transpose2 xs)
-- transpose2 x = zipWith (zipWith ) x []

-- multMat :: Num a => Matrix a -> Matrix a -> Matrix a 
-- multMat x y = f x (transpose2 y)
--     where 

    
multMat xs ys = [[sum (zipWith (*) x y) | y <- transpose ys] | x <- xs]

seriesMat m = iterate f m 
    where f x = addMat m (multMat m x)

seriesMat2 :: Matrix Int -> [Matrix Int]
seriesMat2 m = scanl1 f (inf m)
-- seriesMat2 m = scanl f m (inf m)
    where inf m = m : inf m 
          f x y = addMat (multMat m x) y

