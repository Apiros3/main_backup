

applyEach :: [a -> b] -> a -> [b]
applyEach [] _ = []
applyEach (f:xs) x = f x : applyEach xs x 

applyEach' :: [a -> b] -> a -> [b] 
applyEach' x y = foldr (\f lis -> f y : lis) [] x 

applyEach'' :: [t -> b] -> t -> [b]
applyEach'' x y = map (\f -> f y) x


unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x = if (p x) then [] else (h x) : unfold p h t (t x)

map' f = unfold (\x -> length x == 0) (f.head) (tail) 

iterate' = unfold (const False) id 

int2bits :: Int -> [Int]
int2bits = unfold (== 0) (\x -> if x `mod` 2 == 0 then 0 else 1) (\x -> div x 2)

