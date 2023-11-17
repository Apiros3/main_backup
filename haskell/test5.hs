


data Foo = LLeft | RRight 

instance Eq Foo where 
    (==) LLeft LLeft = True
    (==) RRight RRight = True 
    (==) _ _ = False  

-- f :: Eq Foo => Foo -> Foo -> Bool 
f :: Foo -> Bool
f a = a == LLeft 

data Tree a = Nil | Fork a (Tree a) (Tree a) 

instance Eq a => Eq (Tree a) where 
    Nil == Nil = True 
    Fork a b c == Fork d e f = (a == d && b == e && c == f)
    _ == _ = False 


g :: Eq a => a -> Tree a -> Tree a -> Bool
g x a b = Fork x a a == b 


f' :: (b -> a -> b) -> b -> a -> a -> b
f' g' x' = g' . g' x' 


