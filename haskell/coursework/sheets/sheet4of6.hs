-- import Distribution.Simple.Build (build)
-- import Data.Tree (flatten)


-- 7.1
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

cp2 :: [[a]] -> [[a]]
cp2 = foldr(\xs yss -> concat [map(x:) yss | x <- xs]) [[]]
    
cp3 :: [[a]] -> [[a]]
cp3 = foldr(\xs yss -> [x:ys | x <- xs, ys <- yss]) [[]]

-- 7.2
cols :: [[a]] -> [[a]]
cols    [xs]        = [[x] | x <- xs]
cols    (xs:xss)    = zipWith (:) xs (cols xss)

cols2 :: [[a]] -> [[a]]
-- cols2   []          = repeat[]
-- cols2   (xs:xss)    =  zipWith (:) xs (cols2 xss)
-- cols2 = foldr(\xs yss -> zipWith (:) xs yss) (repeat [])
cols2 = foldr(zipWith (:)) (repeat [])


-- 6.13

data MTree a =  Branch (MTree a) (MTree a)  |
                Leaf [a]      

build' :: [a] -> MTree a
build' [] = Leaf []
build' [x] = Leaf [x]
build' xs = Branch (build' l) (build' r)
    where (l,r) = splitAt (length xs `div` 2) xs

flatten' :: Ord a => MTree a -> [a]
flatten' (Leaf xs) = xs 
flatten' (Branch x (Branch ys zs)) = flatten' (Branch x (Leaf (flatten' (Branch ys zs))))
flatten' (Branch (Branch ys zs) x) = flatten' (Branch (Leaf (flatten' (Branch ys zs))) x)
flatten' (Branch (Leaf []) (Leaf xs)) = xs 
flatten' (Branch (Leaf xs) (Leaf [])) = xs 
flatten' (Branch (Leaf (x:xs)) (Leaf (y:ys)))   | x < y     = x:flatten' (Branch (Leaf xs) (Leaf (y:ys)))
                                                | otherwise = y:flatten' (Branch (Leaf (x:xs)) (Leaf ys))

-- this part does not work
build2 :: [a] -> Either [a] [a]
build2 []   = Left []
build2 [x]  = Left [x]
build2 xs   = Right xs

unfold' :: ([a] -> Either [a] [a]) -> [a] -> MTree a
unfold' f xs = case f xs of 
        Left a  -> Leaf a 
        Right b -> Branch (unfold' f l) (unfold' f r) 
            where (l,r) = splitAt (length b `div` 2) b


-- fold' f z (Leaf x) = f x z         
-- fold' f 

-- unfold' :: ([a] -> Either [a] [a]) -> [a] -> MTree a 
-- unfold' f xs = case f xs of 
--         Left a  -> Leaf
--         Right b -> Branch (unfold' f c) (unfold' f d)
--             where (c,d) = splitAt (length xs `div` 2) xs
     
-- build' :: [a] -> Either [a] [a]
-- build' []   = Left []
-- build' [x]  = Left [x]
-- build' xs   = Right xs

-- fold' :: (Either [a] [a] -> [a]) -> MTree a -> [a]


-- flatten' :: Either [a] [a] -> [a]
-- flatten' Left 


-- build2 :: [a] -> MTree a
-- build2 = unfold' build'

-- flatten2 :: Ord a => MTree a -> [a]
-- flatten2 = fold' flatten' 


-- 8.1
ljustify :: Int -> String -> String
ljustify 0 x        = x
ljustify n []       = " " ++ ljustify (n-1) []
ljustify n (x:xs)   = x : ljustify (n-1) xs

rjustify :: Int -> String -> String
rjustify n s = reverse (ljustify n (reverse s))


-- 8.2
type Matrix a = [[a]]

scale :: Num a => a -> Matrix a -> Matrix a
scale a = map (map (a*))  

dot :: Num a => [a] -> [a] -> a 
dot m n = sum (zipWith (*) m n)

add :: Num a => Matrix a -> Matrix a -> Matrix a 
add = zipWith (zipWith (+)) 

mul :: Num a => Matrix a -> Matrix a -> Matrix a 
mul n m = [map (dot xs) (cols m) | xs <- n]

table :: Show a => Matrix a -> String
table m = plt (cols [map (ljustify (len xs)) xs | xs <- cols (tostr m)])
    where   len xs  = maximum [length x | x <- xs]
            plt     = unlines . map unwords 
            tostr m = map (map show) m