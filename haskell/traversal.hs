


data Tree a = Leaf a | Fork a (Tree a) (Tree a) deriving(Show)


fold :: (a -> b -> b -> b) -> (a -> b) -> Tree a -> b
fold f g (Leaf x) = g x 
fold f g (Fork a b c) = f a (fold f g b) (fold f g c)

unfold :: (b -> Either a (a,b,b)) -> b -> Tree a
unfold p x = case p x of 
    Left l -> Leaf l 
    Right (a,l,r) -> Fork a (unfold p l) (unfold p r)


preorder :: Tree a -> [a]
preorder xs = f xs []
    where f (Leaf x) ys = x : ys 
          f (Fork a b c) ys = a : f b (f c ys)

inorder :: Tree a -> [a]
inorder xs = f xs []
    where f (Leaf x) ys = x : ys 
          f (Fork a b c) ys = f b (a:f c ys)

postorder :: Tree a -> [a]
postorder xs = f xs []
    where f (Leaf x) ys = x : ys 
          f (Fork a b c) ys = f b (f c (a:ys))
    
ttree :: Tree Int 
ttree = Fork 1 (Fork 2 (Leaf 3) (Leaf 4)) (Fork 5 (Leaf 6) (Fork 7 (Leaf 8) (Leaf 9)))


-- maketree :: [a] -> Tree a 
-- maketree = unfold f 
--     where f [x] = Left x 
--           f (x:xs) = Right (x, l, r) 
--                 where (l,r) = splitAt (length xs `div` 2) xs
