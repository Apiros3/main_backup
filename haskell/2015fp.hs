




data Op = Add | Sub | Mul | Div deriving(Show)
data Expr = Num Int | App Op Expr Expr deriving(Show)

type Polish = [Elem]
type RPolish = [Elem]

data Elem = Number Int | Operator Op deriving(Show)

convert :: Expr -> RPolish 
convert xs = f xs []
    where f (Num x) ys = (Number x) : ys 
          f (App a b c) ys = f b (f c (Operator a : ys))

-- evalP :: Polish -> Int 
-- evalP

-- convert :: Expr -> RPolish 
-- convert (Num x) = [Number x]
-- convert (App x y z) = 


type State = String 
type Rule = (Char, String)

exampleRules :: [Rule]
exampleRules = [('A', "BC"), ('B', "AC"), ('C', "AB")]

applyRule :: [Rule] -> Char -> String 
applyRule [] x = [x]
applyRule ((x,y):xs) z  
    | x == z = y 
    | otherwise = applyRule xs z 

step :: [Rule] -> State -> State 
step x xs = concat (map (applyRule x) xs)

runSystem :: (State, [Rule]) -> [State]
runSystem (x,y) = takeWhile (/= "") (iterate (step y) x)

isPrefix :: Eq a => [a] -> [a] -> Bool 
isPrefix x y = (foldr (&&) True (zipWith (==) x y)) && (length x <= length y)

findStates :: [State] -> String -> [(State, Integer)]
findStates xs str = f 0 xs 
    where f _ [] = []
          f i (x:xs)   
                | isPrefix str x  = (x,i) : f (i+1) xs 
                | otherwise       = f (i+1) xs 


nattrip :: [(Int,Int,Int)]
nattrip = concat (map f [3..])
    where f x = [(a,b,x-a-b) | a <- [1..x], b <- [1..x-a]]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = f xs 
    where f [] = []
          f (y:ys) 
             | y `mod` x == 0  = f ys 
             | otherwise = y : f ys  

primes :: [Int]
primes = map head (iterate sieve [2..])

primtrip :: [(Int, Int, Int)]
primtrip = filter g (filter f nattrip)
    where f (a,b,c) = a*a + b*b == c*c && a < b 
          g (a,b,c) = and (map h lis)
             where lis = takeWhile (<= a) primes
                   h x = length (filter (\y -> mod y x == 0) [a,b,c]) <= 1



-- cat = foldr f id 
--     where f 

