import System.Win32.Exception.Unsupported (removed)

data Natural = Zero | Succ Natural 

convert :: Natural -> Integer 
convert Zero = 0 
convert (Succ x) = 1 + convert x 

addition :: Natural -> Natural -> Natural 
addition Zero x = x 
addition (Succ y) x = addition y (Succ x)

multiply :: Natural -> Natural -> Natural 
multiply Zero x = Zero 
multiply (Succ y) x = addition x (multiply y x)

expon :: Natural -> Natural -> Natural 
expon _ Zero = Succ Zero 
expon x (Succ y) = multiply x (expon x y)

foldNat :: (a -> a) -> a -> Natural -> a
foldNat f e Zero = e
foldNat f e (Succ m) = f (foldNat f e m)

f1 = foldNat (+1) 0 
f2 x = foldNat (+(convert x)) 0 
f3 x = foldNat (*(convert x)) 1 

twoFibs 0 = (0,1)
twoFibs n = (v,u+v)
    where (u,v) = twoFibs (n-1)

fib :: Natural -> Natural
fib = fst . foldNat (\(u,v) -> (v,addition u v)) (Zero, Succ Zero)



invert :: [(a,a)] -> [(a,a)]
invert [] = []
invert ((x,y):xs) = (y,x) : invert xs 

removedup :: Eq a => [a] -> [a]
removedup [] = []
removedup (x:xs) = x : removedup (filter (/= x) xs)

relcomp :: Eq a => [(a,a)] -> [(a,a)] -> [(a,a)]
relcomp ((x,y):xs) ys = removedup (f ys ++ relcomp xs ys)  
    where f = (map (\(u,v) -> (x,v))) . (filter (\(u,v) -> y == u))
relcomp [] _ = []

transclose :: Eq a => [(a,a)] -> [(a,a)]
transclose r = removedup (concat (f [] (iterate (relcomp r) r)))
    where f :: Eq a => [[(a,a)]] -> [[(a,a)]] -> [[(a,a)]]
          f [] (x:xs) = f [x] xs 
          f xs (y:ys)
                | filter (g y) xs == [] = f (y:xs) ys 
                | otherwise             = xs 
                    where g :: Eq a => [(a,a)] -> [(a,a)] -> Bool 
                          g x y = and (zipWith (==) x y)



unfold' :: (t -> Bool) -> (t -> b) -> (t -> t) -> t -> [b]
-- unfold' p f g x = if p x then [] else f x : unfold p f g (g x)
unfold' p f g = map f . takeWhile (not . p) . iterate g


data RTree a = Node a [RTree a]

foldRTree :: (a -> [b] -> b) -> RTree a -> b 
foldRTree f (Node x y) = f x (map (foldRTree f) y)

dft :: RTree a -> [a]
dft (Node x y) = x : concat (map dft y)

dft' :: RTree a -> [a]
dft' = foldRTree f 
    where f x xss = x:concat xss 

dft'' :: RTree a -> [a]
dft'' z = unfold' p f g [z]  
    where p x = null x    
          f (Node x y : ys) = x 
          g (Node x y : ys) = y ++ ys 
          
bft'' :: RTree a -> [a]
bft'' z = unfold' p f g [z]  
    where p x = null x    
          f (Node x y : ys) = x 
          g (Node x y : ys) = ys ++ y

et = Node 1 [ Node 2 [Node 3 [],
    Node 4 []],
    Node 5 [],
    Node 6 [Node 7 []]]

