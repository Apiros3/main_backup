-- import Data.List.NonEmpty (unfold)
import Data.List

data QTree a = Nil | Fork (QTree a) a (QTree a)
-- elems in left tree < a <= elems in right tree 

foldQ :: b -> (b -> a -> b -> b) -> QTree a -> b 
foldQ f g Nil = f 
foldQ f g (Fork xs y zs) = g (foldQ f g xs) y (foldQ f g zs) 

unfoldQ :: (b -> Maybe b) -> (b -> (b,a,b)) -> b -> QTree a 
unfoldQ f g x = case f x of 
    Nothing -> Nil 
    Just xs -> Fork (unfoldQ f g u) h (unfoldQ f g v)
        where (u,h,v) = g xs

flatten :: QTree a -> [a]
flatten = foldQ [] (\x y z -> x ++ [y] ++ z)

build :: Ord a => [a] -> QTree a 
build = unfoldQ f g 
    where f [] = Nothing 
          f x = Just x 
          g xs = (u,head xs,v)
            where u = filter (< head xs) (tail xs)
                  v = filter (>= head xs) (tail xs) 
            
qsort :: Ord a => [a] -> [a]
qsort = flatten.build 
