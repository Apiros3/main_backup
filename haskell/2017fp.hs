

merge (x:xs) (y:ys) 
    | x < y = x : merge (xs) (y:ys)
    | x > y = y : merge (x:xs) ys 
    | otherwise = x : merge xs ys 
merge [] y = y 
merge x [] = x 

hamming = f [1]
    where f (x:xs) = x : f (merge xs [2*x,3*x,5*x])


hamming' = 1 : foldr merge [] [map (2*) hamming', map (3*) hamming', map (5*) hamming']

hamming'' :: [Int] -> [Int]
hamming'' xs = 1 : foldr merge [] xss 
    where xss = map (\x -> map (x*) (hamming'' xs)) xs 



type Grid = Matrix Int 
type Matrix a = [[a]]

choices :: Grid -> Matrix [Int]
choices xss = [[f x | x <- xs] | xs <- xss]
    where f 0 = [1..9]
          f z = [z]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:yss) = [x : ys | x <- xs, ys <- cp yss]

cp' :: [[a]] -> [[a]]
cp' = foldr f [[]]
    where f xs yss = [x : ys | x <- xs, ys <- yss]


expand :: Matrix [a] -> [Matrix a]
expand [] = [[]]
expand (xs:yss) = [x : ys | x <- cp xs, ys <- expand yss]


-- solve = filter complete . expand . choices 

-- solve = filter complete . expand . f . choices
--      where f xs 
--              | prune xs == xs  = xs 
--              | otherwise = f (prune xs)






