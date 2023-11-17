

zero = 0:zero 
one = 1:zero 

integral :: [Float] -> [Float]
integral xs = 0 : zipWith (/) xs [1..]


sin' :: [Float]
sin' = zipWith (+) (0:one) (integral (integral sin'))

cos' :: [Float]
cos' = zipWith (+) one (integral (integral cos'))

inv :: [Float] -> [Float]
inv = map (\x -> 1/x)




data Match = Match String Int String Int 
type Team = String 

em :: [[Match]]
em = [ [ Match "Rovers" 2 "United" 5,
    Match "City" 0 "Athletic" 3,
    Match "Corinthians" 0 "Albion" 0 ],
    [ Match "Rovers" 2 "Athletic" 2,
    Match "United" 10 "Thistle" 0,
    Match "Corinthians" 0 "City" 3 ] ]

teams :: [[Match]] -> [Team] 
teams = removedup.findstr.concat  
    where   findstr [] = []  
            findstr ((Match a _ c _):xs) = a:c: findstr xs 
            removedup [] = [] 
            removedup (x:xs) = x : removedup (dropif x xs)
                where dropif _ [] = []
                      dropif x (y:ys) 
                        | x == y = dropif x ys 
                        | otherwise = y : dropif x ys 

data Results = Results Team [(Int,Int)] deriving(Show)

extract :: [[Match]] -> [Results]
extract lis = resultify (teams lis) (concat lis) 
    where resultify [] _ = []
          resultify (x:xs) ys = Results x (f x ys) : resultify xs ys  
             where f m [] = []
                   f m (Match a b c d : zs)
                        | m == a  = (b,d) : f m zs 
                        | m == c  = (d,b) : f m zs 
                        | otherwise = f m zs 


type Weight = (Int,Int)

weight :: Results -> Weight 
weight (Results _ xs) = foldr (add.f) (0,0) xs
    where f (x,y)
            | x > y = (3,x-y)
            | x == y = (1,x-y)
            | otherwise = (0,x-y)
          add (a,b) (c,d) = (a+c,b+d)
          