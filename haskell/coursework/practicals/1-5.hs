
factor3 :: Integer -> (Integer, Integer)
factor3 n = factorFrom3 2 n

factorFrom3 :: Integer -> Integer -> (Integer, Integer)
factorFrom3 m n | r == 0    = (m,q)
                | q < m     = (n,1)
                | m == 2    = factorFrom3 3 n
                | otherwise = factorFrom3 (m+2) n
    where (q,r) = n `divMod` m




