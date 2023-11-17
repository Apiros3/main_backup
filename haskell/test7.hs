

f :: (b1 -> a -> b2) -> (t -> b2 -> b1) -> t -> b1 -> a -> a -> b2
f g h x y = g . h x . g y 