
unfold' :: (a -> Maybe (b,a)) -> a -> [b]
unfold' f x = case f x of 
    Nothing -> []
    Just (y,ys) -> y:unfold' f ys

unfold'' :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold'' f g h x = case f x of 
    True -> []
    False -> g x : unfold'' f g h (h x)

iter :: (a -> a) -> a -> [a]
iter f = unfold'' (const False) id f 

iter' :: (a -> a) -> a -> [a]
iter' f = unfold' (\x -> Just(x, f x))