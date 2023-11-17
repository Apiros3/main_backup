-- 9.1
data Nat = Zero | Succ Nat 

int :: Nat -> Int 
int Zero        = 0 
int (Succ x)    = 1 + int x 

nat :: Int -> Nat 
nat 0           = Zero 
nat x           = Succ (nat (x-1))

add, mul, pow, tet :: Nat -> Nat -> Nat 
add x Zero          = x
add x (Succ y)      = add (Succ x) y 

mul x Zero          = Zero 
mul x (Succ Zero)   = x 
mul x (Succ y)      = mul x y `add` x

pow x Zero          = Succ Zero 
pow x (Succ y)      = pow x y `mul` x 

tet x Zero          = Succ Zero 
tet x (Succ y)      = x `pow` tet x y 

-- 9.2 
-- foldNat :: (func1) -> (func2) -> Nat -> Int
foldNat :: (a -> a) -> a -> Nat -> a 
foldNat l r Zero        = r 
foldNat l r (Succ x)    = l (foldNat l r x)

-- unfoldNat :: (func1) -> Int -> Nat
unfoldNat :: (a -> Either a a) -> a -> Nat 
unfoldNat f x = case f x of 
    Left v          -> Zero 
    Right v         -> Succ(unfoldNat f v) 

int2 :: Nat -> Int 
int2 = foldNat (+1) 0

nat2 :: Int -> Nat 
nat2 = unfoldNat f
    where   f :: Int -> Either Int Int
            f 0 = Left 0  
            f x = Right (x-1) 
 
add2, mul2, pow2, tet2 :: Nat -> Nat -> Nat 
add2 = foldNat Succ

mul2 x = foldNat (\y -> x `add2` y) Zero

pow2 x = foldNat (\y -> x `mul2` y)  (Succ Zero) 

tet2 x = foldNat (\y -> x `pow2` y) (Succ Zero) 

-- 10.4
data Liste a = Snoc (Liste a) a | Lin 

cat :: Liste a -> Liste a -> Liste a 
cat x Lin = x 
cat x (Snoc y z) = Snoc (cat x y) z

-- folde :: (func l) (func r) Liste a -> a 
folde :: (a -> b -> b) -> b -> Liste a -> b
folde l r Lin           = r
folde l r (Snoc xs x)   = l x (folde l r xs) 

cat2 :: Liste a -> Liste a -> Liste a
cat2 = folde (flip Snoc)

list :: Liste a -> [a] 
list = reverse . folde (:) []

liste :: [a] -> Liste a 
liste = foldr (flip Snoc) Lin . reverse

loop' :: (a -> b -> b) -> b -> Liste a -> b
loop' l r Lin           = r
loop' l r (Snoc xs x)    = loop' l (l x r) xs 

list2 :: Liste a -> [a]
list2 = loop' (:) []

liste2 :: [a] -> Liste a
liste2 = foldl Snoc Lin

-- 10.5
unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f xs = case f xs of 
    Nothing         -> []
    Just (y, ys)    -> unfoldr f ys ++ [y]

unfolde :: (b -> Maybe (a, b)) -> b -> Liste a
unfolde f xs = case f xs of 
    Nothing        -> Lin  
    Just (x,y)     -> Snoc (unfolde f y) x

list3 :: Liste a -> [a]
list3 = unfoldr f 
    where   f Lin           = Nothing
            f (Snoc xs x)   = Just (x, xs)

liste3 :: [a] -> Liste a 
liste3 = unfolde f
    where   f []    = Nothing
            f x     = Just (last x, init x)