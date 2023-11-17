
factor :: Integer -> (Integer, Integer)
factor n = factorFrom 2 n

factorFrom :: Integer -> Integer -> (Integer, Integer)
factorFrom m n | r == 0    = (m,q)
               | otherwise = factorFrom (m+1) n
    where (q,r) = n `divMod` m

